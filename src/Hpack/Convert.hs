{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Hpack.Convert where

import qualified Data.ByteString.Char8                 as BS
import           Data.Char
import           Data.Monoid
import           Prelude                               ()
import           Prelude.Compat

import           Data.List.Split                       (splitOn)
import           Data.Maybe
import qualified Data.Version                          as Version
import qualified Distribution.Compiler                 as Compiler
import qualified Distribution.InstalledPackageInfo     as Cabal
import qualified Distribution.Package                  as Cabal
import qualified Distribution.PackageDescription       as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec.Error             as Cabal
import qualified Distribution.Pretty                   as Cabal
import qualified Distribution.Simple                   as Cabal
import qualified Distribution.Text                     as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.CondTree            as Cabal
import qualified Distribution.Version                  as Cabal
import           Hpack.Config                          hiding (package)
import           Text.PrettyPrint                      (fsep, (<+>))

-- * Public API

-- | Reads a 'Package' from cabal's 'GenericPackageDescription' representation
-- of a @.cabal@ file
fromPackageDescription :: Cabal.GenericPackageDescription -> Package
fromPackageDescription Cabal.GenericPackageDescription{..} =
    let Cabal.PackageDescription{..} = packageDescription
    in
    Package { packageName = Cabal.unPackageName (Cabal.pkgName package)
            , packageVersion = Cabal.prettyShow (Cabal.pkgVersion package)
            , packageSynopsis = nullNothing synopsis
            , packageDescription = nullNothing description
            , packageHomepage = nullNothing homepage
            , packageBugReports = nullNothing bugReports
            , packageCategory = nullNothing category
            , packageStability = nullNothing stability
            , packageAuthor = maybe [] parseCommaSep (nullNothing author)
            , packageMaintainer = maybe [] parseCommaSep (nullNothing maintainer)
            , packageCopyright = maybe [] parseCommaSep (nullNothing copyright)
            , packageLicense = Just (show $ Cabal.pretty (either Cabal.licenseFromSPDX id licenseRaw))
            , packageLicenseFile = listToMaybe licenseFiles
            , packageTestedWith =
                    map toUpper .
                    show .
                    fsep . map (\(f, vr) -> Cabal.pretty f <> Cabal.pretty vr ) <$>
                    nullNothing testedWith
            , packageFlags =
                    map (\Cabal.MkFlag{..} ->
                             let fn = Cabal.unFlagName flagName
                             in Flag { flagName = fn
                                     , flagDescription =
                                             nullNothing flagDescription
                                     , flagManual = flagManual
                                     , flagDefault = flagDefault
                                     })
                    genPackageFlags
            , packageExtraSourceFiles = extraSrcFiles
            , packageDataFiles = dataFiles
            , packageSourceRepository = fromSourceRepos sourceRepos
            , packageLibrary = fromCondLibrary condLibrary
            , packageExecutables = fromCondExecutables condExecutables
            , packageTests = fromCondTestSuites condTestSuites
            , packageBenchmarks = fromCondBenchmarks condBenchmarks
            }

-- | Reads a 'Package' from a @.cabal@ manifest string
fromPackageDescriptionString :: String -> Either ConvertError Package
fromPackageDescriptionString pkgStr =
    case Cabal.parseGenericPackageDescriptionMaybe (BS.pack pkgStr) of
        Nothing   -> Left (ConvertCabalParseError (Cabal.PError undefined "oh no"))
        Just gpkg -> Right (fromPackageDescription gpkg)

instance Eq Cabal.PError where
  (Cabal.PError _ s1) == (Cabal.PError _ s2) = s1 == s2

data ConvertError = ConvertCabalParseError Cabal.PError
  deriving(Show, Eq)

-- data ConvertWarning = CWIgnoreSection String
--                     | CWIgnoreCondition String
--                     | CWIgnoreSourceRepo Cabal.SourceRepo
--                     | CWSourceRepoWithoutUrl Cabal.SourceRepo

-- * Private functions for converting each section

fromSourceRepos :: [Cabal.SourceRepo] -> Maybe SourceRepository
fromSourceRepos [] = Nothing
fromSourceRepos (_repo@Cabal.SourceRepo{..}:_more) =
    -- (
    Just SourceRepository { sourceRepositoryUrl = fromMaybe "" repoLocation
                          -- TODO - this is broken (?)
                          , sourceRepositorySubdir = fmap processDir repoSubdir
                          }
    -- TODO - Warnings
    -- , case repoLocation of
    --       Nothing -> [CWSourceRepoWithoutUrl repo]
    --       _ -> []
    --   ++ map CWIgnoreSourceRepo more
    -- )

fromDependency :: Cabal.Dependency -> Dependency
fromDependency (Cabal.Dependency pn vr _) | vr == Cabal.anyVersion =
    Dependency (show (Cabal.pretty pn)) Nothing
fromDependency (Cabal.Dependency pn vr _) =
    Dependency (show (Cabal.pretty pn <+> Cabal.pretty vr)) Nothing

fromLegacyDependency :: Cabal.LegacyExeDependency -> Dependency
fromLegacyDependency (Cabal.LegacyExeDependency pn vr) | vr == Cabal.anyVersion =
    Dependency pn Nothing
fromLegacyDependency (Cabal.LegacyExeDependency pn vr) =
    Dependency (show (pn ++ show vr)) Nothing

fromCondLibrary :: Maybe (Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Library) -> Maybe (Section Library)
fromCondLibrary mcondLibrary = do
    condLibrary@(Cabal.CondNode Cabal.Library{libBuildInfo} _ components) <- mcondLibrary
    l <- libFromCondLibrary condLibrary
    return (sectionWithBuildInfo l libBuildInfo)
        { sectionConditionals = map fromCondComponentHasBuildInfo components
        }

fromCondExecutables :: [(Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Executable)] -> [Section Executable]
fromCondExecutables = map fromCondExecutableTup

fromCondTestSuites :: [(Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.TestSuite)] -> [Section Executable]
fromCondTestSuites = mapMaybe fromCondTestSuiteTup

fromCondBenchmarks :: [(Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Benchmark)] -> [Section Executable]
fromCondBenchmarks = mapMaybe fromCondBenchmarkTup

fromCondExecutableTup :: (Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Executable) -> Section Executable
fromCondExecutableTup etup@(_, Cabal.CondNode Cabal.Executable{buildInfo} _ components) =
    let e = exeFromCondExecutableTup etup
    in (sectionWithBuildInfo e buildInfo)
       { sectionConditionals = map fromCondComponentHasBuildInfo components
       }

fromCondTestSuiteTup :: (Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.TestSuite) -> Maybe (Section Executable)
fromCondTestSuiteTup ttup@(_, Cabal.CondNode Cabal.TestSuite{testBuildInfo} _ components) = do
    te <- testExeFromCondExecutableTup ttup
    return (sectionWithBuildInfo te testBuildInfo)
       { sectionConditionals = map fromCondComponentHasBuildInfo components
       }

fromCondBenchmarkTup :: (Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Benchmark) -> Maybe (Section Executable)
fromCondBenchmarkTup btup@(_, Cabal.CondNode Cabal.Benchmark{benchmarkBuildInfo} _ components) = do
    be <- benchExeFromCondExecutableTup btup
    return (sectionWithBuildInfo be benchmarkBuildInfo)
       { sectionConditionals = map fromCondComponentHasBuildInfo components
       }

-- * Conditional Mapping
class HasBuildInfo a where
    getBuildInfo :: a -> Cabal.BuildInfo

instance HasBuildInfo Cabal.Library where
    getBuildInfo Cabal.Library{libBuildInfo} = libBuildInfo

instance HasBuildInfo Cabal.Executable where
    getBuildInfo Cabal.Executable{buildInfo} = buildInfo

instance HasBuildInfo Cabal.TestSuite where
    getBuildInfo Cabal.TestSuite{testBuildInfo} = testBuildInfo

instance HasBuildInfo Cabal.Benchmark where
    getBuildInfo Cabal.Benchmark{benchmarkBuildInfo} = benchmarkBuildInfo

fromCondHasBuildInfo :: HasBuildInfo a => Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] a -> Section ()
fromCondHasBuildInfo (Cabal.CondNode hbi _ components) =
    let bi = getBuildInfo hbi
    in (sectionWithBuildInfo () bi)
       { sectionConditionals = map fromCondComponentHasBuildInfo components
       }

fromCondComponentHasBuildInfo :: (HasBuildInfo a)
    => Cabal.CondBranch Cabal.ConfVar [Cabal.Dependency] a
    -> Conditional
fromCondComponentHasBuildInfo (Cabal.CondBranch cond ifTree elseTree) =
    Conditional { conditionalCondition = fromCondition cond
                , conditionalThen = fromCondHasBuildInfo ifTree
                , conditionalElse = fromCondHasBuildInfo <$> elseTree
                }

fromCondition :: Cabal.Condition Cabal.ConfVar -> String
fromCondition (Cabal.Var c) = case c of
    Cabal.OS os -> "os(" ++ show (Cabal.pretty os) ++ ")"
    Cabal.Flag fl -> "flag(" ++ Cabal.unFlagName fl ++ ")"
    Cabal.Arch ar -> "arch(" ++ show (Cabal.pretty ar) ++ ")"
    Cabal.Impl cc vr -> "impl(" ++ show (Cabal.pretty cc <+> Cabal.pretty vr)  ++ ")"
fromCondition (Cabal.CNot c) = "!(" ++ fromCondition c ++ ")"
fromCondition (Cabal.COr c1 c2) = "(" ++ fromCondition c1 ++ ") || (" ++ fromCondition c2 ++ ")"
fromCondition (Cabal.CAnd c1 c2) = "(" ++ fromCondition c1 ++ ") && (" ++ fromCondition c2 ++ ")"
fromCondition (Cabal.Lit b) = show b


-- * Private helpers

-- | Builds a 'Package' 'Section' from a data entity and a 'BuildInfo' entity
sectionWithBuildInfo :: a -> Cabal.BuildInfo -> Section a
sectionWithBuildInfo d Cabal.BuildInfo{..} =
    Section { sectionData = d
            , sectionSourceDirs = processDirs hsSourceDirs
            , sectionDependencies = map fromDependency targetBuildDepends
            , sectionDefaultExtensions = map (show . Cabal.pretty)
                                             defaultExtensions
            , sectionOtherExtensions = map (show . Cabal.pretty) otherExtensions
            , sectionGhcOptions = map (\l -> case words l of
                                              []  -> ""
                                              [x] -> x
                                              _   -> ensureQuoted l) $
                                  fromMaybe [] $
                                  lookup Compiler.GHC (Cabal.perCompilerFlavorToList options)
            , sectionGhcProfOptions = fromMaybe [] $
                lookup Compiler.GHC (Cabal.perCompilerFlavorToList profOptions)
            , sectionCppOptions = cppOptions
            , sectionCCOptions = ccOptions
            , sectionCSources = cSources
            , sectionExtraLibDirs = processDirs extraLibDirs
            , sectionExtraLibraries = extraLibs
            , sectionIncludeDirs = processDirs includeDirs
            , sectionInstallIncludes = installIncludes
            , sectionLdOptions = ldOptions
            , sectionBuildable = Just buildable
            -- TODO ^^ ????
            , sectionConditionals = []
            -- TODO ^^ ????
            , sectionBuildTools = map fromLegacyDependency buildTools
            }

libFromCondLibrary :: Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Library -> Maybe Library
libFromCondLibrary (Cabal.CondNode (Cabal.Library{..}) _ _) = do
    let Cabal.BuildInfo{..} = libBuildInfo
    return Library { libraryExposed = Just libExposed
                   , libraryExposedModules = map (show . Cabal.pretty)
                                                 exposedModules
                   , libraryOtherModules = map (show . Cabal.pretty) otherModules
                   , libraryReexportedModules = map (show . Cabal.pretty)
                                                    reexportedModules
                   }

exeFromCondExecutableTup :: (Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Executable) -> Executable
exeFromCondExecutableTup (name, Cabal.CondNode Cabal.Executable{..} _ _) =
    Executable { executableName = Cabal.unUnqualComponentName name
               , executableMain = modulePath
               , executableOtherModules = map (show . Cabal.pretty)
                                              (Cabal.otherModules buildInfo)
               }

testExeFromCondExecutableTup :: (Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.TestSuite) -> Maybe Executable
testExeFromCondExecutableTup (name, Cabal.CondNode Cabal.TestSuite{..} _ _) =
    case testInterface of
        Cabal.TestSuiteExeV10 _ mainIs -> Just
            Executable { executableName = Cabal.unUnqualComponentName name
                       , executableMain = mainIs
                       , executableOtherModules = map (show . Cabal.pretty)
                                                  (Cabal.otherModules testBuildInfo)
                       }
        _ -> Nothing

benchExeFromCondExecutableTup :: (Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] Cabal.Benchmark) -> Maybe Executable
benchExeFromCondExecutableTup (name, Cabal.CondNode Cabal.Benchmark{..} _ _) =
    case benchmarkInterface of
        Cabal.BenchmarkExeV10 _ mainIs -> Just
            Executable { executableName = Cabal.unUnqualComponentName name
                       , executableMain = mainIs
                       , executableOtherModules = map (show . Cabal.pretty)
                                                  (Cabal.otherModules benchmarkBuildInfo)
                       }
        _ -> Nothing

-- | Returns Nothing if a list is empty and Just the list otherwise
--
-- >>> nullNothing []
-- Nothing
-- >>> nullNothing [1, 2, 3]
-- Just [1, 2, 3]
nullNothing :: [a] -> Maybe [a]
nullNothing s = const s <$> listToMaybe s

processDirs :: [String] -> [String]
processDirs = map processDir

-- | Replaces @.@ with @./.@
--
-- See https://github.com/sol/hpack/issues/119
processDir :: String -> String
processDir "." = "./."
processDir d   = d

-- | Parse comma separated list, stripping whitespace
parseCommaSep :: String -> [String]
parseCommaSep s =
  -- separate on commas
  -- strip leading and trailing whitespace
  fmap trimEnds (splitOn "," s)

-- | Trim leading and trailing whitespace
trimEnds :: String -> String
trimEnds = f . f
  where f = reverse . dropWhile isSpace

ensureQuoted :: String -> String
ensureQuoted l =
  if isQuoted l
    then l
    else "\"" <> l <> "\""

isQuoted :: String -> Bool
isQuoted s = testQuote '\'' || testQuote '\"'
  where
    testQuote q = head s == q && last s == q
