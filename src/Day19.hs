module Day19 (Input, datafile, parser, part1, part2) where


import Control.Parallel.Strategies (parMap, rseq)
--import Data.Attoparsec.Text ((<?>))
import Text.Trifecta ((<?>))
import qualified Text.Trifecta as A
import qualified Data.Set as S
import ParserUtils ( eol, int, string )

type Input = [Blueprint]

data Blueprint = Blueprint
  { number :: !Int
  , ore :: !Int
  , clay :: !Int
  , obsidian :: !(Int, Int)
  , geode :: !(Int, Int)
  } deriving stock (Show, Eq)

datafile :: FilePath
datafile = "data/Day19.txt"

-- ################################################################################

parser :: A.Parser Input
parser = A.many $ do
    _ <- string "Blueprint " <?> "blueprint"
    n <- int
    _ <- string ": Each ore robot costs " <?> "ore cost"
    ore <- int
    _ <- string " ore. Each clay robot costs " <?> "clay cost"
    clay <- int
    _ <- string " ore. Each obsidian robot costs " <?> "obsidian cost1"
    obs_ore <- int
    _ <- string " ore and " <?> "obsidian cost2"
    obs_clay <- int
    _ <- string " clay. Each geode robot costs " <?> "obsidian cost1"
    geode_ore <- int
    _ <- string " ore and " <?> "obsidian cost2"
    geode_obs <- int
    _ <- string " obsidian." <?> "last bit"
    _ <- eol
    pure $ Blueprint { number = n, ore = ore, clay = clay, obsidian = (obs_ore, obs_clay), geode = (geode_ore, geode_obs) }

-- ################################################################################

-- 1264
part1 :: Input -> Int
part1 xs = sum ys
  where
    ys = parMap rseq (\b -> number b * solve 24 b initSt) xs

solve :: Int -> Blueprint -> St -> Int
solve limit blue st0 = go 0 (S.singleton (0,0,0,0)) [st0]
  where
    go t _    sts | t == limit = maximum (map geodeamount sts)
    go t seen sts = go (t+1) (S.union seen (S.fromList (map rep sts'))) sts'
        where
            sts' = map earn sts ++
                   filter (\x -> rep x `S.notMember` seen) (ordNub (concatMap (actions blue) sts))

rep :: St -> (Int,Int,Int,Int)
rep st = (orebots st, claybots st, obsidianbots st, geodebots st)

earn :: St -> St
earn st =
    st { oreamount      = oreamount      st + orebots      st,
         clayamount     = clayamount     st + claybots     st,
         obsidianamount = obsidianamount st + obsidianbots st,
         geodeamount    = geodeamount    st + geodebots    st }

ordNub :: Ord a => [a] -> [a]
ordNub xs = foldr f (const []) xs S.empty
  where
    f x rec seen =
      case rec <$> S.alterF (\old -> (old, True)) x seen of
        (True,  ys) -> ys
        (False, ys) -> x : ys

actions :: Blueprint -> St -> [St]
actions b st =
    [st' { oreamount = oreamount st' - geoCostOre, obsidianamount = obsidianamount st' - geoCostObs, geodebots = geodebots st' + 1 }
        | geoCostOre <= oreamount st, geoCostObs <= obsidianamount st -- affordable
        ] <++
    [st' { oreamount = oreamount st' - obsCostOre, clayamount = clayamount st' - obsCostClay, obsidianbots = obsidianbots st' + 1 }
        | obsCostOre <= oreamount st, obsCostClay <= clayamount st -- affordable
        , obsidianbots st < geoCostObs                      -- useful
        ] <++
    [st' { oreamount = oreamount st' - clay b, claybots = claybots st' + 1 }
        | clay b <= oreamount st                        -- affordable
        , claybots st < obsCostClay                    -- useful
        ] ++
    [st' { oreamount = oreamount st' - ore b, orebots = orebots st' + 1 }
        | ore b <= oreamount st                         -- affordable
        , orebots st < oreCostMax                      -- useful
        ]
    where
        st' = earn st
        oreCostMax = (ore b `max` clay b `max` obsCostOre `max` geoCostOre)
        (geoCostOre, geoCostObs) = geode b
        (obsCostOre, obsCostClay) = obsidian b

(<++) :: [a] -> [a] -> [a]
[] <++ xs = xs
xs <++ _ = xs

infixr 5 <++

-- part1 :: Input -> Int
-- part1 xs = trace (show ys) $
--     sum (map (\(a,b) -> a * b) ys)
--   where
--     ys = parMap rseq (\x -> (number x, quality 24 x)) xs

data St = St
  { orebots :: !Int
  , oreamount :: !Int
  , claybots :: !Int
  , clayamount :: !Int
  , obsidianbots :: !Int
  , obsidianamount :: !Int
  , geodebots :: !Int
  , geodeamount :: !Int
  } deriving stock (Show, Eq, Ord)

initSt :: St
initSt = St { orebots = 1, oreamount = 0, claybots = 0, clayamount = 0, obsidianbots = 0, obsidianamount = 0, geodebots = 0, geodeamount = 0 }

-- quality :: Int -> Blueprint -> Int
-- quality n b = maximum $ map geodeamount xs
--   where
--     xs = bfs step1 (initSt b n)

-- step1 :: St -> [St]
-- step1 s | (depth s == 0)             = []
--         | length doBuildGeode > 0    = doBuildGeode  -- always build a geode cracker if you can
--         | length doBuildObsidian > 0 = doBuildObsidian -- otherwise, always build an obsidian bot if you can (maybe?)
--         | otherwise                  = [s'] <> doBuildOre <> doBuildClay
--   where
--     s' = s { oreamount = oreamount s + orebots s, clayamount = clayamount s + claybots s, obsidianamount = obsidianamount s + obsidianbots s, geodeamount = geodeamount s + geodebots s, depth = depth s - 1 }
--     doBuildOre = if oreamount s >= ore (blueprint s) then [s' { oreamount = oreamount s' - ore (blueprint s), orebots = orebots s' + 1 }] else []
--     doBuildClay = if oreamount s >= clay (blueprint s) then [s' {oreamount = oreamount s' - clay (blueprint s), claybots = claybots s' + 1 }] else []
--     (oreo, clayo) = obsidian (blueprint s)
--     doBuildObsidian = if oreamount s >= oreo && clayamount s >= clayo then [s' {oreamount = oreamount s' - oreo, clayamount = clayamount s' - clayo, obsidianbots = obsidianbots s' + 1 }] else []
--     (oreg, obsg) = geode (blueprint s)
--     doBuildGeode = if oreamount s >= oreg && obsidianamount s >= obsg then [s' {oreamount = oreamount s' - oreg, obsidianamount = obsidianamount s' - obsg, geodebots = geodebots s' + 1 }] else []

-- ################################################################################

part2 :: Input -> Int
part2 xs = product ys
  where
    ys = parMap rseq (\b -> solve 32 b initSt) (take 3 xs)