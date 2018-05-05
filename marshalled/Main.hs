{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms       #-}

module Main where

import           Control.Exception
import           Data.Bits
import           Graphics.Vulkan
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Wrapped
import           Say
import           System.Exit

main :: IO ()
main = do
  vulkanInstance <- createMyInstance `catch` \(VulkanException e) -> do
    sayErrString ("Failed to create Vulkan Instance: " ++ show e)
    exitFailure
  print vulkanInstance
  destroyInstance vulkanInstance Nothing

createMyInstance :: IO VkInstance
createMyInstance =
  let createInfo = InstanceCreateInfo
        { vkPNext                   = Nothing
        , vkFlags                   = zeroBits
        , vkPApplicationInfo        = Just ApplicationInfo
          { vkPNext              = Nothing
          , vkPApplicationName   = Just "vulkan-example"
          , vkApplicationVersion = 0
          , vkPEngineName        = Nothing
          , vkEngineVersion      = 0
          , vkApiVersion         = VK_MAKE_VERSION 1 0 0
          }
        , vkPpEnabledLayerNames     = []
        , vkPpEnabledExtensionNames = []
        }
  in createInstance createInfo Nothing
