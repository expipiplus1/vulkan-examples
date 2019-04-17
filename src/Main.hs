{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms       #-}

module Main where

import           Data.Bits
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Vulkan.C

main :: IO ()
main = do
  vulkanInstance <- createInstance
  destroyInstance vulkanInstance

createInstance :: IO VkInstance
createInstance = withCString "vulkan-example" $ \namePtr ->
  with VkApplicationInfo
      { vkSType              = VK_STRUCTURE_TYPE_APPLICATION_INFO
      , vkPNext              = nullPtr
      , vkPApplicationName   = namePtr
      , vkApplicationVersion = 1
      , vkPEngineName        = namePtr
      , vkEngineVersion      = 0
      , vkApiVersion         = VK_MAKE_VERSION 1 0 0
      }
    $ \appInfo ->
        with VkInstanceCreateInfo
            { vkSType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
            , vkPNext                   = nullPtr -- castPtr debugInfo
            , vkFlags                   = VkInstanceCreateFlags zeroBits
            , vkPApplicationInfo        = appInfo
            , vkEnabledLayerCount       = 0
            , vkPPEnabledLayerNames     = nullPtr
            , vkEnabledExtensionCount   = 0
            , vkPPEnabledExtensionNames = nullPtr
            }
          $ \instInfo -> alloca $ \instPtr -> do
              err <- vkCreateInstance instInfo nullPtr instPtr
              print err
              inst <- peek instPtr
              print =<< (withCString "vkCreateDevice" $ vkGetInstanceProcAddr inst)
              print =<< (withCString "vkGetInstanceProcAddr" $ vkGetInstanceProcAddr inst)
              peek instPtr

destroyInstance :: VkInstance -> IO ()
destroyInstance inst = vkDestroyInstance inst nullPtr
