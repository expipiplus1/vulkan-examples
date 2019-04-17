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
import qualified Graphics.Vulkan.C.Dynamic as Vk

main :: IO ()
main = do
  vulkanInstance <- createInstance
  cmds <- Vk.initInstanceCmds vulkanInstance
  destroyInstance cmds vulkanInstance

createInstance :: IO VkInstance
createInstance = withCString "vulkan-example" $ \namePtr ->
  with VkApplicationInfo
      { vkSType              = VK_STRUCTURE_TYPE_APPLICATION_INFO
      , vkPNext              = nullPtr
      , vkPApplicationName   = namePtr
      , vkApplicationVersion = 1
      , vkPEngineName        = namePtr
      , vkEngineVersion      = 0
      , vkApiVersion         = 0
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
              err <- Vk.createInstance instInfo nullPtr instPtr
              inst <- peek instPtr
              print err
              instCmds <- Vk.initInstanceCmds inst
              print instCmds
              pure inst

destroyInstance :: Vk.InstanceCmds -> VkInstance -> IO ()
destroyInstance cmds inst = Vk.destroyInstance cmds inst nullPtr
