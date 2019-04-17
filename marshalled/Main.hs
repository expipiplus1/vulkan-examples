{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Exception
import           Graphics.Vulkan
import           Say
import           System.Exit

main :: IO ()
main = do
  print =<< enumerateAllInstanceExtensionProperties Nothing
  withInstance
      zero
        { vkPApplicationInfo        = Just zero
                                        { vkPApplicationName = Just "vulkan-example"
                                        , vkApiVersion = VK_API_VERSION_1_1
                                        }
        }
      Nothing
      print
    `catch` \(e :: VulkanException) -> do
              sayErrString ("Vulkan error: " ++ displayException e)
              exitFailure
