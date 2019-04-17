{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NumDecimals              #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeOperators            #-}

module Main where

import           Debug.Trace
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Extra
import           Data.Bits
import qualified Data.ByteString               as BS
import           Data.Coerce
import           Data.Foldable
import           Data.Int
import           Data.Maybe
import           Data.String                              ( IsString )
import           Data.Text
import           Data.Traversable
import           Data.Vector                              ( Vector )
import qualified Data.Vector                   as V
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.IO.Exception                         ( IOErrorType
                                                            ( OtherError
                                                            )
                                                          , IOException(..)
                                                          )
import           Graphics.Vulkan
import qualified Graphics.Vulkan.C             as C
import           Graphics.Vulkan.C.Core10.Core            ( VkBool32(..) )
import qualified Graphics.Vulkan.C.Dynamic     as Dyn
import           Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
import           Graphics.Vulkan.Marshal.SomeVkStruct
import           Graphics.Vulkan.Marshal.Utils            ( withArray )
import           Say
import qualified SDL
import qualified SDL.Internal.Types            as SDL
import qualified SDL.Video.Vulkan              as SDL
import           System.Exit
import           Text.Show.Pretty


main :: IO ()
main = withSDL . withVulkanWindow appName width height $ \window inst -> do
  SDL.showWindow window

withVulkanWindow
  :: Text -> Int -> Int -> (SDL.Window -> Instance -> IO a) -> IO a
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
            go window inst

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
  let requiredLayers =
        [ "VK_LAYER_LUNARG_standard_validation"
        ]
      requiredExtensions =
        V.fromList $ VK_EXT_DEBUG_UTILS_EXTENSION_NAME : windowExtensions
  pure zero
    { vkPNext = Just (SomeVkStruct debugUtilsMessengerCreateInfo)
    , vkPApplicationInfo = Just zero { vkPApplicationName = Just appName
                                     , vkApiVersion = VK_MAKE_VERSION 1 1 0
                                     }
    , vkPpEnabledLayerNames     = requiredLayers
    , vkPpEnabledExtensionNames = requiredExtensions
    }

{-
main :: IO ()
main = withSDL $ withWindow appName 1960 1080 $ \w -> do
  SDL.showWindow w
  say "Available extensions:"
  traverse_ (sayShow . vkExtensionName)
    =<< enumerateAllInstanceExtensionProperties Nothing
  traverse_ sayShow =<< enumerateAllInstanceLayerProperties
  ci <- windowInstanceCreateInfo w
  withInstance ci Nothing $ \inst -> do
    sayErr "Instance Created"
    debugReportCallback <- createDebugReportCallbackEXT inst
                                                        debugCallbackCI
                                                        Nothing
    physDevices <- enumerateAllPhysicalDevices inst
    sayErr
      $  "Found "
      <> pack (show (V.length physDevices))
      <> " physical devices"
    (physDevice, graphicsQueueFamily) <- pickBestDevice physDevices
    let qci = zero { vkQueueFamilyIndex = graphicsQueueFamily
                   , vkPQueuePriorities = [1]
                   }
        requiredLayers =
          [ "VK_LAYER_LUNARG_standard_validation"
          , "VK_LAYER_LUNARG_core_validation"
          ]
        dci = zero
          { vkPQueueCreateInfos       = [qci]
          , vkPpEnabledLayerNames     = requiredLayers
          , vkPpEnabledExtensionNames = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
          , vkPEnabledFeatures        = Just zeroPhysicalDeviceFeatures
          }
    withDevice physDevice dci Nothing $ \device -> do
      graphicsQueue <- getDeviceQueue device graphicsQueueFamily 0
      surface       <- getSDLWindowSurface w inst
      -- TODO: Create queue families properly, at the moment it's
      -- assumed that the graphics and present queues are the same
      surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR physDevice surface
      formats       <- getAllPhysicalDeviceSurfaceFormatsKHR physDevice surface
      presentModes  <- getAllPhysicalDeviceSurfacePresentModesKHR physDevice
                                                                  surface
      let
        scCI :: SwapchainCreateInfoKHR
        scCI = zero
          { vkSurface          = surface
          , vkMinImageCount    =
            (vkMinImageCount (surfaceCaps :: SurfaceCapabilitiesKHR)) + 1
          , vkImageFormat      = C.VK_FORMAT_B8G8R8A8_UNORM
          , vkImageColorSpace  = C.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
          , vkImageExtent      = Extent2D 1080 1920
          , vkImageArrayLayers = 1
          , vkImageUsage       = C.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
          , vkImageSharingMode = C.VK_SHARING_MODE_EXCLUSIVE
          , vkPresentMode      = C.VK_PRESENT_MODE_FIFO_KHR
          }
      withSwapchainKHR device scCI Nothing $ \swapchain -> do
        swapchainImages <- getAllSwapchainImagesKHR device swapchain
        withArray
            (\image -> withImageView
              device
              zero
                { vkImage            = image
                , vkViewType         = C.VK_IMAGE_VIEW_TYPE_2D
                , vkFormat           = C.VK_FORMAT_B8G8R8A8_UNORM
                , vkComponents       = ComponentMapping
                                         C.VK_COMPONENT_SWIZZLE_IDENTITY
                                         C.VK_COMPONENT_SWIZZLE_IDENTITY
                                         C.VK_COMPONENT_SWIZZLE_IDENTITY
                                         C.VK_COMPONENT_SWIZZLE_IDENTITY
                , vkSubresourceRange = ImageSubresourceRange
                                         { vkAspectMask     =
                                           C.VK_IMAGE_ASPECT_COLOR_BIT
                                         , vkBaseMipLevel   = 0
                                         , vkLevelCount     = 1
                                         , vkBaseArrayLayer = 0
                                         , vkLayerCount     = 1
                                         }
                }
              Nothing
            )
            swapchainImages
          $ \imageViews -> do

              let presentInfo =
                    zero { vkPSwapchains = [swapchain], vkPImageIndices = [0] }
              -- queuePresentKHR graphicsQueue presentInfo

              mainLoop

pickBestDevice :: Vector PhysicalDevice -> IO (PhysicalDevice, Word32)
pickBestDevice devices = do
  let
    getGraphicsQueueFamily :: PhysicalDevice -> IO (Maybe Word32)
    getGraphicsQueueFamily d = do
      _properties <- getPhysicalDeviceProperties d
      qfps        <- V.indexed <$> getAllPhysicalDeviceQueueFamilyProperties d
      let isGraphicsQueue q =
            ((/= zeroBits) . (.&. C.VK_QUEUE_GRAPHICS_BIT) . vkQueueFlags $ q)
              && ((> 0) . vkQueueCount $ q)
          graphicsQueueIndex = case V.filter (isGraphicsQueue . snd) qfps of
            [] -> Nothing
            xs -> Just (fst $ V.head xs)
      pure (fromIntegral <$> graphicsQueueIndex)
  withQueues <- catMaybes <$> traverse
    (\x -> fmap (x, ) <$> getGraphicsQueueFamily x)
    (V.toList devices)
  case withQueues of
    [] -> do
      sayErr "No suitable Vulkan device found"
      exitFailure
    x : _ -> pure x

-- | Create an instance and device with a graphics queue for an SDL window
withGraphicsDevice :: SDL.Window -> (Device -> IO a) -> IO a
withGraphicsDevice w go = do
  ci <- windowInstanceCreateInfo w
  withInstance ci Nothing $ \inst -> do
    sayErr "Instance Created"
    debugReportCallback <- createDebugReportCallbackEXT inst
                                                        debugCallbackCI
                                                        Nothing
    physDevices <- enumerateAllPhysicalDevices inst
    sayErr
      $  "Found "
      <> pack (show (V.length physDevices))
      <> " physical devices"
    (physDevice, graphicsQueueFamily) <- pickBestDevice physDevices
    let qci = zero { vkQueueFamilyIndex = graphicsQueueFamily
                   , vkPQueuePriorities = [1]
                   }
        requiredLayers =
          [ "VK_LAYER_LUNARG_standard_validation"
          , "VK_LAYER_LUNARG_core_validation"
          ]
        dci = zero
          { vkPQueueCreateInfos       = [qci]
          , vkPpEnabledLayerNames     = requiredLayers
          , vkPpEnabledExtensionNames = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
          , vkPEnabledFeatures        = Just zeroPhysicalDeviceFeatures
          }
    withDevice physDevice dci Nothing go

mainLoop :: IO ()
mainLoop = whileM $ do
  quit <- maybe True (not . isQuitEvent) <$> SDL.pollEvent
  if quit
    then pure True
    else do
      threadDelay 16e3
      pure False



----------------------------------------------------------------
-- Vulkan Utils
----------------------------------------------------------------

zeroPhysicalDeviceFeatures :: PhysicalDeviceFeatures
zeroPhysicalDeviceFeatures = PhysicalDeviceFeatures
  { vkRobustBufferAccess                   = False
  , vkFullDrawIndexUint32                  = False
  , vkImageCubeArray                       = False
  , vkIndependentBlend                     = False
  , vkGeometryShader                       = False
  , vkTessellationShader                   = False
  , vkSampleRateShading                    = False
  , vkDualSrcBlend                         = False
  , vkLogicOp                              = False
  , vkMultiDrawIndirect                    = False
  , vkDrawIndirectFirstInstance            = False
  , vkDepthClamp                           = False
  , vkDepthBiasClamp                       = False
  , vkFillModeNonSolid                     = False
  , vkDepthBounds                          = False
  , vkWideLines                            = False
  , vkLargePoints                          = False
  , vkAlphaToOne                           = False
  , vkMultiViewport                        = False
  , vkSamplerAnisotropy                    = False
  , vkTextureCompressionETC2               = False
  , vkTextureCompressionASTC_LDR           = False
  , vkTextureCompressionBC                 = False
  , vkOcclusionQueryPrecise                = False
  , vkPipelineStatisticsQuery              = False
  , vkVertexPipelineStoresAndAtomics       = False
  , vkFragmentStoresAndAtomics             = False
  , vkShaderTessellationAndGeometryPointSize = False
  , vkShaderImageGatherExtended            = False
  , vkShaderStorageImageExtendedFormats    = False
  , vkShaderStorageImageMultisample        = False
  , vkShaderStorageImageReadWithoutFormat  = False
  , vkShaderStorageImageWriteWithoutFormat = False
  , vkShaderUniformBufferArrayDynamicIndexing = False
  , vkShaderSampledImageArrayDynamicIndexing = False
  , vkShaderStorageBufferArrayDynamicIndexing = False
  , vkShaderStorageImageArrayDynamicIndexing = False
  , vkShaderClipDistance                   = False
  , vkShaderCullDistance                   = False
  , vkShaderFloat64                        = False
  , vkShaderInt64                          = False
  , vkShaderInt16                          = False
  , vkShaderResourceResidency              = False
  , vkShaderResourceMinLod                 = False
  , vkSparseBinding                        = False
  , vkSparseResidencyBuffer                = False
  , vkSparseResidencyImage2D               = False
  , vkSparseResidencyImage3D               = False
  , vkSparseResidency2Samples              = False
  , vkSparseResidency4Samples              = False
  , vkSparseResidency8Samples              = False
  , vkSparseResidency16Samples             = False
  , vkSparseResidencyAliased               = False
  , vkVariableMultisampleRate              = False
  , vkInheritedQueries                     = False
  }

    -}

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
