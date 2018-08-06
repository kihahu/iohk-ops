{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults #-}

module Types where

import qualified Data.Aeson            as AE
import           Data.Aeson            ((.=), (.:))
import qualified Data.ByteString.Char8 as BS.C8
import           Data.Csv              (FromField (..))
import           Data.String
import           Data.Text
import           Data.Word             (Word16)
import           Data.Yaml             (FromJSON (..), ToJSON (..))
import           GHC.Generics          hiding (from, to)


-- * Elementary types
--
newtype AccessKeyId  = AccessKeyId  { fromAccessKeyId  :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype Arg          = Arg          { fromArg          :: Text   } deriving (IsString, Show)
newtype Branch       = Branch       { fromBranch       :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype ConfigurationKey = ConfigurationKey { fromConfigurationKey :: Text } deriving (IsString, Show)
newtype Commit       = Commit       { fromCommit       :: Text   } deriving (Eq, FromJSON, Generic, Show, IsString, ToJSON)
newtype Exec         = Exec         { fromExec         :: Text   } deriving (IsString, Show)
newtype EnvVar       = EnvVar       { fromEnvVar       :: Text   } deriving (IsString, Show)
newtype JournaldTimeSpec = JournaldTimeSpec { fromJournaldTimeSpec :: Text   } deriving (Show, IsString)
newtype NixParam     = NixParam     { fromNixParam     :: Text   } deriving (FromJSON, Generic, Show, IsString, Eq, Ord, AE.ToJSONKey, AE.FromJSONKey)
newtype NixHash      = NixHash      { fromNixHash      :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype NixAttr      = NixAttr      { fromAttr         :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype NixopsCmd    = NixopsCmd    { fromCmd          :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype NixopsDepl   = NixopsDepl   { fromNixopsDepl   :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype Org          = Org          { fromOrg          :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype Region       = Region       { fromRegion       :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype Zone         = Zone         { fromZone         :: Text   } deriving (FromJSON, Generic, Show, IsString)
newtype URL          = URL          { fromURL          :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype FQDN         = FQDN         { fromFQDN         :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype IP           = IP           { getIP            :: Text   } deriving (Show, Generic, FromField)
newtype PortNo       = PortNo       { fromPortNo       :: Int    } deriving (FromJSON, Generic, Show, ToJSON)
newtype Username     = Username     { fromUsername     :: Text   } deriving (FromJSON, Generic, Show, IsString, ToJSON)
newtype ApplicationVersion = ApplicationVersion { getApplicationVersion :: Text } deriving (FromJSON, IsString, Show, Eq, Generic, ToJSON)

-- * Arch
data Arch = Linux64 | Mac64 | Win64 deriving (Show, Read, Eq, Generic)

instance FromJSON Arch
instance ToJSON Arch

type ApplicationVersionKey = Arch -> Text

formatArch :: Arch -> Text
formatArch Linux64 = "Linux"
formatArch Mac64 = "macOS"
formatArch Win64 = "Windows"

-- | A map of values indexed by Arch.
-- Maybe "type ArchMap a = Arch -> a" would be better but this works.
data ArchMap a = ArchMap { linux64 :: !a, mac64 :: !a, win64 :: !a }
  deriving (Show, Read, Eq, Generic, Functor)

-- | Construct an arch map using fixed values.
mkArchMap :: a -- ^ Linux value
          -> a -- ^ macOS value
          -> a -- ^ Windows value
          -> ArchMap a
mkArchMap l m w = ArchMap l m w

-- | Construct an arch map with a lookup function.
archMap :: (Arch -> a) -> ArchMap a
archMap get = ArchMap (get Linux64) (get Mac64) (get Win64)

-- | Get the value for a given arch.
lookupArch :: Arch -> ArchMap a -> a
lookupArch Linux64 (ArchMap a _ _) = a
lookupArch Mac64   (ArchMap _ a _) = a
lookupArch Win64   (ArchMap _ _ a) = a

-- | Returns the map as a list of (Arch, value) pairs.
archMapToList :: ArchMap a -> [(Arch, a)]
archMapToList (ArchMap l m w) = [(Linux64, l), (Mac64, m), (Win64, w)]

-- | Returns the map as a list of (Arch, value) pairs, where not all
-- arches are present.
archMapToList' :: ArchMap (Maybe a) -> [(Arch, a)]
archMapToList' am = [(a, v) | (a, Just v) <- archMapToList am]

-- | Construct an arch map from a list of pairs.
archMapFromList :: [(Arch, a)] -> ArchMap (Maybe a)
archMapFromList = build (ArchMap Nothing Nothing Nothing)
  where
    build am [] = am
    build am (e:es) = build (add e am) es
    add (Linux64, l) (ArchMap _ m w) = ArchMap (Just l) m w
    add (Mac64, m)   (ArchMap l _ w) = ArchMap l (Just m) w
    add (Win64, w)   (ArchMap l m _) = ArchMap l m (Just w)

instance FromJSON a => FromJSON (ArchMap a) where
  parseJSON = AE.withObject "ArchMap" $ \o ->
    mkArchMap <$> o .: "linux" <*> o .: "darwin" <*> o .: "windows"

instance ToJSON a => ToJSON (ArchMap a) where
  toJSON am = AE.object [ "linux" .= lookupArch Linux64 am
                        , "darwin" .= lookupArch Mac64 am
                        , "windows" .= lookupArch Win64 am ]


-- * Flags
--
data BuildNixops      = BuildNixops      | DontBuildNixops    deriving (Bounded, Eq, Ord, Show); instance Flag BuildNixops
data Confirmed        = Confirmed        | Unconfirmed        deriving (Bounded, Eq, Ord, Show); instance Flag Confirmed
data Debug            = Debug            | NoDebug            deriving (Bounded, Eq, Ord, Show); instance Flag Debug
data Serialize        = Serialize        | DontSerialize      deriving (Bounded, Eq, Ord, Show); instance Flag Serialize
data Verbose          = Verbose          | NotVerbose         deriving (Bounded, Eq, Ord, Show); instance Flag Verbose
data ComponentCheck   = ComponentCheck   | NoComponentCheck   deriving (Bounded, Eq, Ord, Show); instance Flag ComponentCheck
data DoCommit         = DoCommit         | DontCommit         deriving (Bounded, Eq, Ord, Show); instance Flag DoCommit
data BuildOnly        = BuildOnly        | NoBuildOnly        deriving (Bounded, Eq, Ord, Show); instance Flag BuildOnly
data DryRun           = DryRun           | NoDryRun           deriving (Bounded, Eq, Ord, Show); instance Flag DryRun
data Validate         = Validate         | SkipValidation     deriving (Bounded, Eq, Ord, Show); instance Flag Validate
data PassCheck        = PassCheck        | DontPassCheck      deriving (Bounded, Eq, Ord, Show); instance Flag PassCheck
data WipeJournals     = WipeJournals     | KeepJournals       deriving (Bounded, Eq, Ord, Show); instance Flag WipeJournals
data WipeNodeDBs      = WipeNodeDBs      | KeepNodeDBs        deriving (Bounded, Eq, Ord, Show); instance Flag WipeNodeDBs
data ResumeFailed     = ResumeFailed     | DontResume         deriving (Bounded, Eq, Ord, Show); instance Flag ResumeFailed
data GenerateKeys     = GenerateKeys     | DontGenerateKeys   deriving (Bounded, Eq, Ord, Show); instance Flag GenerateKeys

deriving instance Eq NodeType
deriving instance Read NodeName
deriving instance AE.ToJSONKey NodeName

fromNodeName :: NodeName -> Text
fromNodeName (NodeName x) = x

class (Bounded a, Eq a) => Flag a where
  toBool :: a -> Bool
  toBool = (== enabled)
  fromBool :: Bool -> a
  fromBool x = if x then minBound else maxBound
  enabled, disabled :: a
  enabled  = minBound
  disabled = maxBound
  opposite :: a -> a
  opposite = fromBool . not . toBool


-- * Topology.hs
--
data NodeAddr a =
    NodeAddrExact BS.C8.ByteString (Maybe Word16)
  | NodeAddrDNS a (Maybe Word16)
  deriving (Show)

newtype NodeName = NodeName Text deriving (Show, Ord, Eq, IsString)

instance FromJSON NodeName where
  parseJSON = fmap NodeName . parseJSON

data NodeOrg = IOHK | CF | Emurgo
    deriving (Bounded, Eq, Enum, Generic, Read, Show)

instance FromJSON NodeOrg
instance ToJSON NodeOrg

data NodeType = NodeCore | NodeRelay | NodeEdge
  deriving (Show)

instance FromJSON NodeType where
  parseJSON = AE.withText "NodeType" $ \typ -> do
      case unpack typ of
        "core"     -> return NodeCore
        "edge"     -> return NodeEdge
        "relay"    -> return NodeRelay
        _otherwise -> fail $ "Invalid NodeType " ++ show typ


-- * Domain
--
data Deployment
  = Every
  | Explorer
  | Infra
  | Nodes
  | ReportServer
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Deployment

data Environment
  = Any               -- ^ Wildcard or unspecified, depending on context.
  | Benchmark
  | Production
  | Staging
  | Development
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Environment

data Target
  = All               -- ^ Wildcard or unspecified, depending on context.
  | AWS
  deriving (Bounded, Eq, Enum, Generic, Read, Show)
instance FromJSON Target
