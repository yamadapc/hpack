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
            , sectionBuildable = Just buildable
            -- TODO ^^ ????
            , sectionConditionals = []
            -- TODO ^^ ????
            , sectionBuildTools = map fromDependency buildTools
            }

fromCondLibrary condLibrary = do
    Cabal.CondNode Cabal.Library{libBuildInfo} _ _ <- condLibrary
    l <- libFromCondLibrary condLibrary
    return (sectionWithBuildInfo l libBuildInfo)

exeFromCondExecutableTup (name, Cabal.CondNode Cabal.Executable{..} _ _) =
    Executable { executableName = name
               , executableMain = modulePath
               , executableOtherModules = map (show . Cabal.disp)
                                              (Cabal.otherModules buildInfo)
               }

testExeFromCondExecutableTup (name, Cabal.CondNode Cabal.TestSuite{..} _ _) =
    case testInterface of
        Cabal.TestSuiteExeV10 _ mainIs -> Just $
            Executable { executableName = name
                       , executableMain = mainIs
                       , executableOtherModules = map (show . Cabal.disp)
                                                  (Cabal.otherModules testBuildInfo)
                       }
        _ -> Nothing

benchExeFromCondExecutableTup (name, Cabal.CondNode Cabal.Benchmark{..} _ _) =
    case benchmarkInterface of
        Cabal.BenchmarkExeV10 _ mainIs -> Just $
            Executable { executableName = name
                       , executableMain = mainIs
                       , executableOtherModules = map (show . Cabal.disp)
                                                  (Cabal.otherModules benchmarkBuildInfo)
                       }
        _ -> Nothing

fromCondExecutableTup etup@(_, Cabal.CondNode Cabal.Executable{buildInfo} _ _) =
    let e = exeFromCondExecutableTup etup
    in sectionWithBuildInfo e buildInfo

fromCondTestSuiteTup ttup@(_, Cabal.CondNode Cabal.TestSuite{testBuildInfo} _ _) = do
    te <- testExeFromCondExecutableTup ttup
    return $ sectionWithBuildInfo te testBuildInfo

fromCondBenchmarkTup btup@(_, Cabal.CondNode Cabal.Benchmark{benchmarkBuildInfo} _ _) = do
    be <- benchExeFromCondExecutableTup btup
    return $ sectionWithBuildInfo be benchmarkBuildInfo

fromCondExecutables = map fromCondExecutableTup

fromCondTestSuites = catMaybes . map fromCondTestSuiteTup

fromCondBenchmarks = catMaybes . map fromCondBenchmarkTup

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
