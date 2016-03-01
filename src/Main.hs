{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Graphics.Vulkan
import Graphics.Vulkan.Version
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.String
import Data.Bits

main :: IO ()
main = do
  vulkanInstance <- createInstance
  destroyInstance vulkanInstance

createInstance =
  withCString "vulkan-example" 
  (\namePtr -> 
  with VkApplicationInfo{ vkSType = VK_STRUCTURE_TYPE_APPLICATION_INFO
                        , vkPNext = nullPtr
                        , vkPApplicationName = namePtr
                        , vkApplicationVersion = 1
                        , vkPEngineName = namePtr
                        , vkEngineVersion = 0
                        , vkApiVersion = vkMakeVersion 1 0 3
                        }
  (\appInfo -> 
  with VkInstanceCreateInfo{ vkSType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                           , vkPNext = nullPtr -- castPtr debugInfo
                           , vkFlags = VkInstanceCreateFlags zeroBits
                           , vkPApplicationInfo = appInfo
                           , vkEnabledLayerCount = 0
                           , vkPpEnabledLayerNames = nullPtr
                           , vkEnabledExtensionCount = 0
                           , vkPpEnabledExtensionNames = nullPtr
                           }
  (\instInfo -> 
  alloca
  (\instPtr -> do
  err <- vkCreateInstance instInfo nullPtr instPtr
  print err
  peek instPtr 
  )))) 
  
destroyInstance inst = vkDestroyInstance inst nullPtr
