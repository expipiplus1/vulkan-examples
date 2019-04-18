{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

module Main where

import           Control.Exception
import           Data.Bits
import           Data.Traversable
import           Data.List                                ( nub )
import qualified Data.ByteString               as BS
import           Data.Word
import           Say
import           System.Exit
import           Data.Maybe
import           Data.Coerce
import           Data.String                              ( IsString )
import           Data.Text                         hiding ( maximum )
import qualified Data.Vector                   as V
import           Foreign.Ptr
import           Graphics.Vulkan
import qualified Graphics.Vulkan.C             as C
import           Graphics.Vulkan.C.Core10.Core            ( VkBool32(..) )
import           Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
import           Graphics.Vulkan.Marshal.SomeVkStruct
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL


main :: IO ()
main =
  withSDL . withVulkanWindow appName width height $ \window dev surface ->
    SDL.showWindow window

withVulkanWindow
  :: Text -> Int -> Int -> (SDL.Window -> Device -> SurfaceKHR -> IO a) -> IO a
withVulkanWindow appName width height go =
  withWindow appName width height $ \window -> do
    instanceCreateInfo <- windowInstanceCreateInfo window
    withInstance instanceCreateInfo Nothing $ \inst ->
      withDebugUtilsMessengerEXT inst debugUtilsMessengerCreateInfo Nothing
        $ \_ -> do
            submitDebugUtilsMessageEXT
              inst
              VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
              VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
              zero { vkPMessage = "Debug Message Test" }
            withSDLWindowSurface inst window $ \surface ->
              withGraphicalDevice inst surface
                $ \dev graphicsQueue presentQueue -> do
                    go window dev surface

appName :: IsString a => a
appName = "vulkan triangle example"

width, height :: Int
width = 1920
height = 1080

-- | InstanceCreateInfo for an SDL window
windowInstanceCreateInfo :: SDL.Window -> IO InstanceCreateInfo
windowInstanceCreateInfo window = do
  windowExtensions <- traverse BS.packCString
    =<< SDL.vkGetInstanceExtensions window
  let requiredLayers = ["VK_LAYER_LUNARG_standard_validation"]
      requiredExtensions =
        V.fromList $ VK_EXT_DEBUG_UTILS_EXTENSION_NAME : windowExtensions
  pure zero
    { vkPNext = Just (SomeVkStruct debugUtilsMessengerCreateInfo)
    , vkPApplicationInfo = Just zero { vkPApplicationName = Just appName
                                     , vkApiVersion = VK_MAKE_VERSION 1 1 0
                                     }
    , vkPpEnabledLayerNames = requiredLayers
    , vkPpEnabledExtensionNames = requiredExtensions
    }

withGraphicalDevice
  :: Instance -> SurfaceKHR -> (Device -> Queue -> Queue -> IO a) -> IO a
withGraphicalDevice inst surface go = do
  (physicalDevice, graphicsQueueFamilyIndex, presentQueueFamilyIndex) <-
    pickGraphicalPhysicalDevice inst surface
  let
    deviceCreateInfo :: DeviceCreateInfo
    deviceCreateInfo = zero
      { vkPQueueCreateInfos =
        V.fromList
          [ zero { vkQueueFamilyIndex = i, vkPQueuePriorities = [1] }
          | i <- nub [graphicsQueueFamilyIndex, presentQueueFamilyIndex]
          ]
      }
  withDevice physicalDevice deviceCreateInfo Nothing $ \dev -> do
    graphicsQueue <- getDeviceQueue2
      dev
      zero { vkQueueFamilyIndex = graphicsQueueFamilyIndex
           , vkFlags            = VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT
           }
    presentQueue <- getDeviceQueue2
      dev
      zero { vkQueueFamilyIndex = presentQueueFamilyIndex
           , vkFlags            = VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT
           }
    go dev graphicsQueue presentQueue

-- | Find the device which has the most memory and a graphics queue family index
pickGraphicalPhysicalDevice
  :: Instance -> SurfaceKHR -> IO (PhysicalDevice, Word32, Word32)
pickGraphicalPhysicalDevice inst surface =
  enumerateAllPhysicalDevices inst >>= \case
    [] -> do
      sayErr "No physical devices found"
      exitFailure
    devs -> do
      let headMay = \case
            [] -> Nothing
            xs -> Just (V.unsafeHead xs)
      annDevs <- for devs $ \dev ->
        (dev, , , , )
          <$> getGraphicsQueueIndices dev
          <*> getPresentQueueIndices dev surface
          <*> deviceHasSwapChain dev
          <*> deviceScore dev
      let
        bestDev =
          snd
            . maximum
            $ [ (score, (dev, g, p))
              | (dev, headMay -> Just g, headMay -> Just p, True, score) <-
                V.toList annDevs
              ]
      pure bestDev
  where
    deviceScore :: PhysicalDevice -> IO Word64
    deviceScore dev = do
      -- may be useful later...
      -- properties <- getPhysicalDeviceProperties dev
      -- features <- getPhysicalDeviceFeatures dev
      heaps <- vkMemoryHeaps <$> getPhysicalDeviceMemoryProperties dev
      let totalSize = sum $ (vkSize :: MemoryHeap -> DeviceSize) <$> heaps
      pure totalSize

    deviceHasSwapChain :: PhysicalDevice -> IO Bool
    deviceHasSwapChain dev = do
      extensions <- enumerateAllDeviceExtensionProperties dev Nothing
      pure $ V.any ((VK_KHR_SWAPCHAIN_EXTENSION_NAME ==) . vkExtensionName)
                   extensions

    getGraphicsQueueIndices :: PhysicalDevice -> IO (V.Vector Word32)
    getGraphicsQueueIndices dev = do
      queueFamilyProperties <- getAllPhysicalDeviceQueueFamilyProperties dev
      let isGraphicsQueue q =
            (C.VK_QUEUE_GRAPHICS_BIT .&&. vkQueueFlags q)
              && (vkQueueCount q > 0)
          graphicsQueueIndices = fromIntegral . fst <$> V.filter
            (isGraphicsQueue . snd)
            (V.indexed queueFamilyProperties)
      pure graphicsQueueIndices

    getPresentQueueIndices
      :: PhysicalDevice -> SurfaceKHR -> IO (V.Vector Word32)
    getPresentQueueIndices dev surface = do
      numQueues <- getNumPhysicalDeviceQueueFamilyProperties2 dev
      let queueIndices = V.generate (fromIntegral numQueues) fromIntegral
      V.filterM (\i -> getPhysicalDeviceSurfaceSupportKHR dev i surface)
                queueIndices

----------------------------------------------------------------
-- Debugging
----------------------------------------------------------------

debugUtilsMessengerCreateInfo :: DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo = zero
  { vkMessageSeverity = -- VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                         -- VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
                        VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                          .|. VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , vkMessageType     = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                        .|. VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                        .|. VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , vkPfnUserCallback = castFunPtrToPtr debugCallbackPtr
  }

foreign import ccall unsafe "DebugCallback.c &debugCallback"
  debugCallbackPtr
  :: FunPtr ( ("messageSeverity" ::: C.VkDebugUtilsMessageSeverityFlagBitsEXT)
           -> ("messageType" ::: C.VkDebugUtilsMessageTypeFlagsEXT)
           -> ("pCallbackData" ::: Ptr C.VkDebugUtilsMessengerCallbackDataEXT)
           -> ("pUserData" ::: Ptr ())
           -> IO VkBool32)

----------------------------------------------------------------
-- SDL helpers
----------------------------------------------------------------

-- | Run something having initialized SDL
withSDL :: IO a -> IO a
withSDL =
  bracket_ (SDL.initialize ([SDL.InitEvents] :: [SDL.InitFlag])) SDL.quit

-- | Create an SDL window and use it
withWindow :: Text -> Int -> Int -> (SDL.Window -> IO a) -> IO a
withWindow title width height = bracket
  (SDL.createWindow
    title
    (SDL.defaultWindow
      { SDL.windowInitialSize = SDL.V2 (fromIntegral width)
                                       (fromIntegral height)
      , SDL.windowVulkan      = True
      }
    )
  )
  SDL.destroyWindow

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  (SDL.Event _ SDL.QuitEvent) -> True
  SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ SDL.KeycodeQ _)))
    -> True
  _ -> False

-- | Get the Vulkan surface for an SDL window
getSDLWindowSurface :: Instance -> SDL.Window -> IO SurfaceKHR
getSDLWindowSurface inst window =
  wordPtrToPtr . fromIntegral <$> SDL.vkCreateSurface
    window
    (coerce (instanceHandle inst))

withSDLWindowSurface :: Instance -> SDL.Window -> (SurfaceKHR -> IO a) -> IO a
withSDLWindowSurface inst window = bracket
  (getSDLWindowSurface inst window)
  (\o -> destroySurfaceKHR inst o Nothing)

----------------------------------------------------------------
-- Bit utils
----------------------------------------------------------------

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
