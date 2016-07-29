{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Convert where

import           Data.Maybe
import           Data.Monoid
import qualified Data.Version                          as Cabal
import qualified Distribution.Compiler                 as Cabal
import qualified Distribution.Package                  as Cabal
import qualified Distribution.PackageDescription       as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Text                     as Cabal
import           Hpack.Config
import           Text.PrettyPrint                      (fsep, (<+>))

data ConvertError = ConvertError
  deriving(Show, Eq)

nullNothing s = const s <$> listToMaybe s

fromDependency (Cabal.Dependency pn vr) =
    Dependency (show (Cabal.disp pn <+> Cabal.disp vr)) Nothing

sectionWithBuildInfo :: a -> Cabal.BuildInfo -> Section a
sectionWithBuildInfo d Cabal.BuildInfo{..} =
    Section { sectionData = d
            , sectionSourceDirs = hsSourceDirs
            , sectionDependencies = map fromDependency targetBuildDepends
            , sectionDefaultExtensions = map (show . Cabal.disp)
                                             defaultExtensions
            , sectionOtherExtensions = map (show . Cabal.disp) otherExtensions
            , sectionGhcOptions = fromMaybe [] $
                lookup Cabal.GHC options
            , sectionGhcProfOptions = fromMaybe [] $
                lookup Cabal.GHC profOptions
            , sectionCppOptions = cppOptions
            , sectionCCOptions = ccOptions
            , sectionCSources = cSources
            , sectionExtraLibDirs = extraLibDirs
            , sectionExtraLibraries = extraLibs
            , sectionIncludeDirs = includeDirs
            , sectionInstallIncludes = installIncludes
            , sectionLdOptions = ldOptions
            , sectionBuildable = Just buildable -- TODO <- ????
            , sectionConditionals = [] -- TODO <- ????
            , sectionBuildTools = map fromDependency buildTools
            }

fromCondLibrary condLibrary = do
    Cabal.CondNode (Cabal.Library{libBuildInfo}) _ _ <- condLibrary
    l <- libFromCondLibrary condLibrary
    return (sectionWithBuildInfo l libBuildInfo)

fromCondExecutables = const []
fromCondTestSuites = const []
fromCondBenchmarks = const []

libFromCondLibrary condLibrary = do
    Cabal.CondNode (Cabal.Library{..}) _ _ <- condLibrary
    let Cabal.BuildInfo{..} = libBuildInfo
    return Library { libraryExposed = Just libExposed
                   , libraryExposedModules = map (show . Cabal.disp)
                                                 exposedModules
                   , libraryOtherModules = map (show . Cabal.disp) otherModules
                   , libraryReexportedModules = map (show . Cabal.disp)
                                                    reexportedModules
                   }

fromPackageDescription :: Cabal.GenericPackageDescription -> Either ConvertError Package
fromPackageDescription Cabal.GenericPackageDescription{..} =
    let Cabal.PackageDescription{..} = packageDescription
    in Right $
    Package { packageName = Cabal.unPackageName (Cabal.pkgName package)
            , packageVersion = Cabal.showVersion (Cabal.pkgVersion package)
            , packageSynopsis = nullNothing synopsis
            , packageDescription = nullNothing description
            , packageHomepage = nullNothing homepage
            , packageBugReports = nullNothing bugReports
            , packageCategory = nullNothing category
            , packageStability = nullNothing stability
            , packageAuthor = maybeToList (nullNothing author)
            , packageMaintainer = maybeToList (nullNothing maintainer)
            , packageCopyright = maybeToList (nullNothing copyright)
            , packageLicense = Just (show (Cabal.disp license))
            , packageLicenseFile = listToMaybe licenseFiles
            , packageTestedWith =
                    show .
                    fsep . map (\(f, vr) -> Cabal.disp f <+> Cabal.disp vr ) <$>
                    nullNothing testedWith
            , packageFlags =
                    map (\Cabal.MkFlag{..} ->
                             let Cabal.FlagName fn = flagName
                             in Flag { flagName = fn
                                     , flagDescription =
                                             nullNothing flagDescription
                                     , flagManual = flagManual
                                     , flagDefault = flagDefault
                                     })
                    genPackageFlags
            , packageExtraSourceFiles = extraSrcFiles
            , packageDataFiles = dataFiles
            , packageSourceRepository = do
                    Cabal.SourceRepo{..} <- listToMaybe sourceRepos
                    return SourceRepository { sourceRepositoryUrl = fromMaybe "" repoLocation
                                            -- TODO - this is broken (?)
                                            , sourceRepositorySubdir = repoSubdir
                                            }
            , packageLibrary = fromCondLibrary condLibrary
            , packageExecutables = fromCondExecutables condExecutables
            , packageTests = fromCondTestSuites condTestSuites
            , packageBenchmarks = fromCondBenchmarks condBenchmarks
            }

fromPackageDescriptionString :: String -> Either ConvertError Package
fromPackageDescriptionString pkgStr =
    case Cabal.parsePackageDescription pkgStr of
        Cabal.ParseFailed _ ->
            Left ConvertError
        Cabal.ParseOk _ gpkg -> fromPackageDescription gpkg
