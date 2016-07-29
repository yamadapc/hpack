module Hpack.ConvertSpec (spec) where

import           Prelude                ()
import           Prelude.Compat

import           Test.Hspec
import           Test.QuickCheck

import           Hpack.Config
import           Hpack.Convert

spec :: Spec
spec = do
  describe "fromPackageDescriptionString" $ do
    describe "simple generated cabal file" $ do
      it "cabal init -m" $ do
        pkgDescription <- readFile "./test/data/cabal-init-minimal.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Just Section { sectionData = Library { libraryExposed = Just True
                                                                                             , libraryExposedModules = []
                                                                                             , libraryOtherModules = []
                                                                                             , libraryReexportedModules = []
                                                                                             }
                                                                     , sectionSourceDirs = ["src"]
                                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                                     , sectionDefaultExtensions = []
                                                                     , sectionOtherExtensions = []
                                                                     , sectionGhcOptions = []
                                                                     , sectionGhcProfOptions = []
                                                                     , sectionCppOptions = []
                                                                     , sectionCCOptions = []
                                                                     , sectionCSources = []
                                                                     , sectionExtraLibDirs = []
                                                                     , sectionExtraLibraries = []
                                                                     , sectionIncludeDirs = []
                                                                     , sectionInstallIncludes = []
                                                                     , sectionLdOptions = []
                                                                     , sectionBuildable = Just True
                                                                     , sectionConditionals = []
                                                                     , sectionBuildTools = []
                                                                     }
                                     , packageExecutables = []
                                     , packageTests = []
                                     , packageBenchmarks = []
                                     }

      it "cabal init -m with executables" $ do
        pkgDescription <- readFile "./test/data/cabal-init-with-executables.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Nothing
                                     , packageExecutables = [
                                             Section { sectionData = Executable { executableName = "hello-world"
                                                                                , executableMain = "HelloWorld.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     , packageTests = []
                                     , packageBenchmarks = []
                                     }

      it "cabal init -m with test-suites" $ do
        pkgDescription <- readFile "./test/data/cabal-init-with-tests.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Nothing
                                     , packageExecutables = [
                                             Section { sectionData = Executable { executableName = "hello-world"
                                                                                , executableMain = "HelloWorld.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     , packageTests = [
                                             Section { sectionData = Executable { executableName = "hello-world-spec"
                                                                                , executableMain = "Spec.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src", "test" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                                      ]
                                     , packageBenchmarks = []
                                     }

      it "cabal init -m with benchmarks" $ do
        pkgDescription <- readFile "./test/data/cabal-init-with-benchmarks.cabal"
        let pkg = fromPackageDescriptionString pkgDescription
        pkg `shouldBe` Right Package { packageName = "cabal-init-minimal"
                                     , packageVersion = "0.1.0.0"
                                     , packageSynopsis = Nothing
                                     , packageDescription = Nothing
                                     , packageHomepage = Nothing
                                     , packageBugReports = Nothing
                                     , packageCategory = Nothing
                                     , packageStability = Nothing
                                     , packageAuthor = [ "Pedro Tacla Yamada" ]
                                     , packageMaintainer = [ "tacla.yamada@gmail.com" ]
                                     , packageCopyright = []
                                     , packageLicense = Just "PublicDomain"
                                     , packageLicenseFile = Nothing
                                     , packageTestedWith = Nothing
                                     , packageFlags = []
                                     , packageExtraSourceFiles = ["ChangeLog.md"]
                                     , packageDataFiles = []
                                     , packageSourceRepository = Nothing
                                     , packageLibrary = Nothing
                                     , packageExecutables = [
                                             Section { sectionData = Executable { executableName = "hello-world"
                                                                                , executableMain = "HelloWorld.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                             ]
                                     , packageTests = [
                                             Section { sectionData = Executable { executableName = "hello-world-spec"
                                                                                , executableMain = "Spec.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src", "test" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                                      ]
                                     , packageBenchmarks = [
                                             Section { sectionData = Executable { executableName = "hello-world-benchmark"
                                                                                , executableMain = "Bench.hs"
                                                                                , executableOtherModules = []
                                                                                }
                                                     , sectionSourceDirs = [ "src", "benchmarks" ]
                                                     , sectionDependencies = [ Dependency "base >=4.8 && <4.9" Nothing ]
                                                     , sectionDefaultExtensions = []
                                                     , sectionOtherExtensions = []
                                                     , sectionGhcOptions = []
                                                     , sectionGhcProfOptions = []
                                                     , sectionCppOptions = []
                                                     , sectionCCOptions = []
                                                     , sectionCSources = []
                                                     , sectionExtraLibDirs = []
                                                     , sectionExtraLibraries = []
                                                     , sectionIncludeDirs = []
                                                     , sectionInstallIncludes = []
                                                     , sectionLdOptions = []
                                                     , sectionBuildable = Just True
                                                     , sectionConditionals = []
                                                     , sectionBuildTools = []
                                                     }
                                                           ]
                                     }
