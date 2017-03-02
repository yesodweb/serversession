{-# LANGUAGE CPP #-}
module Import.NoFoundation
    ( module Import
#if !MIN_VERSION_yaml(0,8,16)
    , loadYamlSettings
#endif
#if !MIN_VERSION_yaml(0,8,17)
    , loadYamlSettingsArgs
#endif
    ) where

import ClassyPrelude.Yesod   as Import
import Model                 as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import


#if !MIN_VERSION_yaml(0,8,16)
loadYamlSettings :: FromJSON settings => [FilePath] -> [Value] -> EnvUsage -> IO settings
loadYamlSettings = loadAppSettings
#endif
#if !MIN_VERSION_yaml(0,8,17)
loadYamlSettingsArgs :: FromJSON settings => [Value] -> EnvUsage -> IO settings
loadYamlSettingsArgs = loadAppSettingsArgs
#endif
