module Game.Client.Resources where

import System.IO.Unsafe

import Graphics.UI.SDL        as SDL
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Keysym as SDL.Keysym

import Game.Shared.Object

-------------
-- Sprites --
-------------
-- |Loads an image from disk
loadImage :: FilePath -- ^Path to image file
          -> SDL.Surface
loadImage path = unsafePerformIO (do 
    sur <- loadBMP path
    return sur)
    
-- |Loads an image from disk, making any purple pixels transparent
loadImageKeyed :: FilePath -- ^Path to image file
               -> SDL.Surface
loadImageKeyed path = unsafePerformIO (do 
    sur <- loadBMP path
    mask <- mapRGB (surfaceGetPixelFormat sur) 255 0 255
    setColorKey sur [SrcColorKey] mask
    return sur)
    
minionImage Blue = loadImageKeyed "Assets/Sprites/minion_blue.bmp"
minionImage Red = loadImageKeyed "Assets/Sprites/minion_red.bmp"
minionProjectileImage = loadImageKeyed "Assets/Sprites/minion_projectile.bmp"

playerImage Blue = loadImageKeyed "Assets/Sprites/player_blue.bmp"
playerImage Red = loadImageKeyed "Assets/Sprites/player_red.bmp"

connectingImage = loadImageKeyed "Assets/Sprites/connecting.bmp"
couldNotConnectImage = loadImageKeyed "Assets/Sprites/connection_failed.bmp"

victoryImage True = loadImageKeyed "Assets/Sprites/victory.bmp"
victoryImage False = loadImageKeyed "Assets/Sprites/defeat.bmp"

backgroundImage = loadImage "Assets/Sprites/map.bmp"

attackIconImage 1 True = loadImage "Assets/Sprites/attack_1_selected.bmp"
attackIconImage 1 False = loadImage "Assets/Sprites/attack_1_idle.bmp"
attackIconImage 2 True = loadImage "Assets/Sprites/attack_2_selected.bmp"
attackIconImage 2 False = loadImage "Assets/Sprites/attack_2_idle.bmp"

turretImage Blue = loadImageKeyed "Assets/Sprites/turret_blue.bmp"
turretImage Red = loadImageKeyed "Assets/Sprites/turret_red.bmp"
turretDeadImage = loadImageKeyed "Assets/Sprites/turret_dead.bmp"
turretProjectileImage = loadImageKeyed "Assets/Sprites/turret_projectile.bmp"
turretProjectileTrailImage = loadImageKeyed "Assets/Sprites/turret_projectile_trail.bmp"

nexusImage Blue 0 = loadImageKeyed "Assets/Sprites/nexus_blue_0.bmp"
nexusImage Blue 1 = loadImageKeyed "Assets/Sprites/nexus_blue_1.bmp"
nexusImage Blue 2 = loadImageKeyed "Assets/Sprites/nexus_blue_2.bmp"
nexusImage Blue 3 = loadImageKeyed "Assets/Sprites/nexus_blue_3.bmp"
nexusImage Blue 4 = loadImageKeyed "Assets/Sprites/nexus_blue_4.bmp"
nexusImage Blue 5 = loadImageKeyed "Assets/Sprites/nexus_blue_5.bmp"
nexusImage Blue 6 = loadImageKeyed "Assets/Sprites/nexus_blue_6.bmp"
nexusImage Blue _ = loadImageKeyed "Assets/Sprites/nexus_blue_0.bmp"
nexusImage Red 0 = loadImageKeyed "Assets/Sprites/nexus_red_0.bmp"
nexusImage Red 1 = loadImageKeyed "Assets/Sprites/nexus_red_1.bmp"
nexusImage Red 2 = loadImageKeyed "Assets/Sprites/nexus_red_2.bmp"
nexusImage Red 3 = loadImageKeyed "Assets/Sprites/nexus_red_3.bmp"
nexusImage Red 4 = loadImageKeyed "Assets/Sprites/nexus_red_4.bmp"
nexusImage Red 5 = loadImageKeyed "Assets/Sprites/nexus_red_5.bmp"
nexusImage Red 6 = loadImageKeyed "Assets/Sprites/nexus_red_6.bmp"
nexusImage Red _ = loadImageKeyed "Assets/Sprites/nexus_red_0.bmp"