# PluginOptEnvConfAnn

`-fplugin-opt` / `$ENV` / `*.conf` / `{-# ANN #-}`

[OptEnvConf](https://github.com/NorfairKing/OptEnvConf) extended for use by GHC plugins.

This is OptEnvConf, you can configure it exactly the same way. It replaces the option handling with `-fplugin-opt` handling (without changing how options are configured) and adds [source annotation](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/extending_ghc.html#source-annotations) handling through additional configuration. Environment variable handling and config file handling are unchanged.
