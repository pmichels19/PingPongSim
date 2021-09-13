module PingPong.Model where

import Transformation

import Data.Geometry hiding (init)
import Data.Geometry.Matrix
import Data.Geometry.Transformation
import Data.Geometry.PolyLine
import Data.Geometry.Polygon

import Data.Ext
import Control.Lens

import Data.Colour



-- * Model of all components of the game.

-- | An Element is either a link or a joint; a robot arm is a list of elements.
--   An arm always consist of revolute Joints which are connected with Links.
data Element = Link  (Colour Float) Float
             | Joint (Colour Float) Float
  deriving (Show, Eq)

-- | Checks whether the provided element is a joint.
isJoint :: Element -> Bool
isJoint (Joint _ _) = True
isJoint _           = False

-- | A robotic arm is a combination of multiple links and joints.
--   The first and last elements should be links; the last link is the bat.
type Arm = [Element]

-- | The current time in seconds since the start of the game.
type GameTime = Float
-- | A Hit stores a moment in time when the ball hit something.
--   This contains both the time of the hit and what got hit.
type Hit = (GameTime, Item)

-- | A combination of location and velocity of the ball.
data BallState = BallState 
  { loc :: Point 2 Float  -- ^ Position of the ball.
  , dir :: Vector 2 Float -- ^ Velocity of the ball.
  } deriving (Show, Eq)

-- | Specifies a single player or character that can play table tennis.
data Player = Player 
  { name      :: String   -- ^ Human-readable player name.
  , arm       :: Arm      -- ^ Description of the player arm.
  , initArm   :: Arm      -- ^ Initial state of the arm.
  , foot      :: Float    -- ^ Distance of the base of the arm from the origin.
    -- | This method is called once at the start of the game.
    --   Can for instance be used for setting up infrastructure for communication.
  , prepare   :: IO ()
    -- | This method is called once at the end of the game.
    --   Used to clean up things created in 'prepare'.
  , terminate :: IO ()
    -- | Provides the action that the robotic arm should perform given
    -- the current state of the world and its own position in space.
  , action    :: GameTime -> Hit -> BallState -> Arm -> IO Motion
    -- | Makes the arm stretch before the start of the game.
  , stretch   :: GameTime -> Arm -> IO Motion
    -- | Makes the robotic arm do a victory dance when it has scored a point.
  , dance     :: GameTime -> Arm -> IO Motion


  }

-- | The motion that each individual joint should make, stored in an array.
-- The speed is defined in radians per second at which joints can move.
type Motion = [Float]

-- * Simulation Data

-- | Data type describing possible things the ball can hit.
data Item 
  = Air         -- ^ The ball is not in collision.
  | Bat Owner   -- ^ The ball is in collision with one of the player's bats.
  | Table Owner -- ^ The ball is in collision with the table last on the specified side.
  | Net         -- ^ The ball is in collision with the net.
  -- | Specifies that the ball hit any other surface denoted by an identifier.
  -- This will be the walls of the room, the ceiling or the floor, the underside
  -- of the table, etc. depending on the location of the ball.
  | Other Int
  deriving (Show, Eq, Ord)

-- | Data type to indicate a player relative to the current player.
data Owner = Self | Opponent
  deriving (Show, Eq, Ord)

-- | Describes the current situation of the field to determine arm behaviour.
data Phase
    -- | The situation when setting up and initialising the robotic arms.
  = BeforeGame Float
    -- | The situation before the players will face off against each other.
    -- This is used to attempt to revert the players back to 0.
  | BeforeRally Float
    -- | The situation where two players are facing off against each other.
    -- This is the main match when they are playing.
  | DuringRally
    -- | The situation after a set has been played and someone scored a point.
    -- This allows for victory dances of each robotic arm.
  | AfterRally Float
    -- | The situation after all sets have been played and a definitive winner
    -- has been chosen! Now the robotic arms can be cleaned up and terminate
    -- removing all remaining data from both players.
  | AfterGame Float
    -- | State to indicate the process should be terminated.
  | GameOver    
  deriving (Eq, Show)

-- | Internal data structure for the current game state.
data State = State 
    -- | The current phase or progress within a game.
  { phase  :: Phase
    -- | The time elapsed since the start of the game.
  , time   :: GameTime
    -- | The number of frames since the start of the game.
  , frame  ::  Int
    -- | The number of points scored by each player.
  , score  :: (Int, Int)
    -- | The location of the ball in space currently.
  , ball   ::  BallState
    -- | The time and location of the last collision.
  , hits   :: [Hit]
    -- | One of the players who participate in this game.
  , p1     ::  Player
    -- | One of the players who participate in this game.
  , p2     ::  Player
    -- | One of the motions until the next frame.
  , m1     ::  Motion
    -- | One of the motions until the next frame.
  , m2     ::  Motion
  }


-- | Geometry of the room.
room :: SimplePolygon () Float
room = Data.Geometry.Polygon.fromPoints $ map (:+ ()) [Point2 (-3) 0, Point2 3 0, Point2 3 6, Point2 (-3) 6]

-- | Geometry of the table.
table :: SimplePolygon () Float
table = Data.Geometry.Polygon.fromPoints $ map (:+ ()) [Point2 1 0.5, Point2 0 0.5, Point2 (-1) 0.5, Point2 (-1) 0.4, Point2 1 0.4]

-- | Geometry of the net.
net :: LineSegment 2 () Float
net = ClosedLineSegment (Point2 0 0.5 :+ ()) (Point2 0 0.6 :+ ())





transformation :: Element -> Transformation 2 Float
transformation (Link _ d) = translation $ Vector2 0 d
transformation (Joint _ a) = rotation a

transformations :: Arm -> [Transformation 2 Float]
transformations = map transformation

globalize :: [Transformation 2 Float] -> [Transformation 2 Float]
globalize ts = scanl (|.|) identity ts

evaluate :: Arm -> [Point 2 Float]
evaluate arm = map (origin .+^) 
             $ map (flip transformBy $ Vector2 0 0) 
             $ globalize $ transformations arm

evaluateP :: Arm -> PolyLine 2 () Float
evaluateP arm = fromPointsUnsafe
              $ map (:+ ())
              $ evaluate arm




armSegments :: Player -> Bool -> [LineSegment 2 () Float]
armSegments p f = 
  let rps = evaluate $ arm p
      trl = translation (Vector2 (foot p) 0)
      scl = scaling (Vector2 (if f then 1 else -1) 1)
      nps = map (transformBy (scl |.| trl)) rps
      fps = map (:+ ()) nps
  in zipWith (OpenLineSegment) (init fps) (tail fps)




 







-- | Change state from the perspective of p1 to the perspective of p2.
flipState :: State -> State
flipState st = st { ball = flipBall $ ball st
                  , hits = map flipHit $ hits st
                  , p1 = p2 st
                  , p2 = p1 st
                  , m1 = flipMotion $ m2 st
                  , m2 = flipMotion $ m1 st
                  }

flipBall :: BallState -> BallState
flipBall st = st { loc = transformBy reflectionH $ loc st
                 , dir = transformBy reflectionH $ dir st
                 }

flipMotion :: Motion -> Motion
flipMotion = map negate

flipHit :: (Float, Item) -> (Float, Item)
flipHit (f, i) = (f, flipItem i)

flipItem :: Item -> Item
flipItem (Bat   o) = Bat   $ flipOwner o
flipItem (Table o) = Table $ flipOwner o
flipItem o         = o

flipOwner :: Owner -> Owner
flipOwner Self = Opponent
flipOwner Opponent = Self

defState :: State
defState = State
  { phase = BeforeGame 2
  , time  = 0
  , frame = 0
  , score = (0, 0)
  , ball  = BallState (Point2 0 1) (Vector2 1 0)
  , hits  = [(0, Bat Opponent)]
  , p1    = undefined
  , p2    = undefined
  , m1    = undefined
  , m2    = undefined
  }

winner :: State -> Player
winner s | fst (score s) > snd (score s) = p1 s
         | otherwise = p2 s

