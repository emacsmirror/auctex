;;; xkcdcolors.el --- AUCTeX style for `xkcdcolors.sty' (v1.0.1)  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2019-10-26
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support for `xkcdcolors.sty' (v1.0.1) from
;; 2019/10/21.  `xkcdcolors.sty' is part of TeXLive.

;;; Code:

(require 'tex)

;; Silence the compiler:
(declare-function LaTeX-add-xcolor-definecolors
                  "xcolor"
                  (&rest xcolor-definecolors))

(defvar LaTeX-xkcdcolors-colornames
  (eval-when-compile
    (mapcar (lambda (x) (concat "xkcd" x))
            '("CloudyBlue"
              "DarkPastelGreen"
              "Dust"
              "ElectricLime"
              "FreshGreen"
              "LightEggplant"
              "NastyGreen"
              "ReallyLightBlue"
              "Tea"
              "WarmPurple"
              "YellowishTan"
              "Cement"
              "DarkGrassGreen"
              "DustyTeal"
              "GreyTeal"
              "MacaroniAndCheese"
              "PinkishTan"
              "Spruce"
              "StrongBlue"
              "ToxicGreen"
              "WindowsBlue"
              "BlueBlue"
              "BlueWithAHintOfPurple"
              "Booger"
              "BrightSeaGreen"
              "DarkGreenBlue"
              "DeepTurquoise"
              "GreenTeal"
              "StrongPink"
              "Bland"
              "DeepAqua"
              "LavenderPink"
              "LightMossGreen"
              "LightSeafoamGreen"
              "OliveYellow"
              "PigPink"
              "DeepLilac"
              "Desert"
              "DustyLavender"
              "PurpleyGrey"
              "Purply"
              "CandyPink"
              "LightPastelGreen"
              "BoringGreen"
              "KiwiGreen"
              "LightGreyGreen"
              "OrangePink"
              "TeaGreen"
              "VeryLightBrown"
              "EggShell"
              "EggplantPurple"
              "PowderPink"
              "ReddishGrey"
              "BabyShitBrown"
              "Liliac"
              "StormyBlue"
              "UglyBrown"
              "Custard"
              "DarkishPink"
              "DeepBrown"
              "GreenishBeige"
              "Manilla"
              "OffBlue"
              "BattleshipGrey"
              "BrownyGreen"
              "Bruise"
              "KelleyGreen"
              "SicklyYellow"
              "SunnyYellow"
              "Azul"
              "Darkgreen"
              "GreenYellow"
              "Lichen"
              "LightLightGreen"
              "PaleGold"
              "SunYellow"
              "TanGreen"
              "Burple"
              "Butterscotch"
              "Toupe"
              "DarkCream"
              "IndianRed"
              "LightLavendar"
              "PoisonGreen"
              "BabyPukeGreen"
              "BrightYellowGreen"
              "CharcoalGrey"
              "Squash"
              "Cinnamon"
              "LightPeaGreen"
              "RadioactiveGreen"
              "RawSienna"
              "BabyPurple"
              "Cocoa"
              "LightRoyalBlue"
              "Orangeish"
              "RustBrown"
              "SandBrown"
              "Swamp"
              "TealishGreen"
              "BurntSiena"
              "Camo"
              "DuskBlue"
              "Fern"
              "OldRose"
              "PaleLightGreen"
              "PeachyPink"
              "RosyPink"
              "LightBluishGreen"
              "LightBrightGreen"
              "LightNeonGreen"
              "LightSeafoam"
              "TiffanyBlue"
              "WashedOutGreen"
              "BrownyOrange"
              "NiceBlue"
              "Sapphire"
              "GreyishTeal"
              "OrangeyYellow"
              "Parchment"
              "Straw"
              "VeryDarkBrown"
              "Terracota"
              "UglyBlue"
              "ClearBlue"
              "Creme"
              "FoamGreen"
              "GreyGreen"
              "LightGold"
              "SeafoamBlue"
              "Topaz"
              "VioletPink"
              "Wintergreen"
              "YellowTan"
              "DarkFuchsia"
              "IndigoBlue"
              "LightYellowishGreen"
              "PaleMagenta"
              "RichPurple"
              "SunflowerYellow"
              "GreenBlue"
              "Leather"
              "RacingGreen"
              "VividPurple"
              "DarkRoyalBlue"
              "Hazel"
              "MutedPink"
              "BoogerGreen"
              "Canary"
              "CoolGrey"
              "DarkTaupe"
              "DarkishPurple"
              "TrueGreen"
              "CoralPink"
              "DarkSage"
              "DarkSlateBlue"
              "FlatBlue"
              "Mushroom"
              "RichBlue"
              "DirtyPurple"
              "Greenblue"
              "IckyGreen"
              "LightKhaki"
              "WarmBlue"
              "DarkHotPink"
              "DeepSeaBlue"
              "Carmine"
              "DarkYellowGreen"
              "PalePeach"
              "PlumPurple"
              "GoldenRod"
              "NeonRed"
              "OldPink"
              "VeryPaleBlue"
              "BloodOrange"
              "Grapefruit"
              "SandYellow"
              "ClayBrown"
              "DarkBlueGrey"
              "FlatGreen"
              "LightGreenBlue"
              "WarmPink"
              "DodgerBlue"
              "GrossGreen"
              "Ice"
              "MetallicBlue"
              "PaleSalmon"
              "SapGreen"
              "Algae"
              "BlueyGrey"
              "GreenyGrey"
              "HighlighterGreen"
              "LightLightBlue"
              "LightMint"
              "RawUmber"
              "VividBlue"
              "DeepLavender"
              "DullTeal"
              "LightGreenishBlue"
              "MudGreen"
              "Pinky"
              "RedWine"
              "ShitGreen"
              "TanBrown"
              "Darkblue"
              "Rosa"
              "Lipstick"
              "PaleMauve"
              "Claret"
              "Dandelion"
              "Orangered"
              "PoopGreen"
              "Ruby"
              "Dark"
              "GreenishTurquoise"
              "PastelRed"
              "PissYellow"
              "BrightCyan"
              "DarkCoral"
              "AlgaeGreen"
              "DarkishRed"
              "ReddyBrown"
              "BlushPink"
              "CamouflageGreen"
              "LawnGreen"
              "Putty"
              "VibrantBlue"
              "DarkSand"
              "PurpleBlue"
              "Saffron"
              "Twilight"
              "WarmBrown"
              "Bluegrey"
              "BubbleGumPink"
              "DuckEggBlue"
              "GreenishCyan"
              "Petrol"
              "Royal"
              "Butter"
              "DustyOrange"
              "OffYellow"
              "PaleOliveGreen"
              "Orangish"
              "Leaf"
              "LightBlueGrey"
              "DriedBlood"
              "LightishPurple"
              "RustyRed"
              "LavenderBlue"
              "LightGrassGreen"
              "LightMintGreen"
              "Sunflower"
              "Velvet"
              "BrickOrange"
              "LightishRed"
              "PureBlue"
              "TwilightBlue"
              "VioletRed"
              "YellowyBrown"
              "Carnation"
              "MuddyYellow"
              "DarkSeafoamGreen"
              "DeepRose"
              "DustyRed"
              "GreyBlue"
              "LemonLime"
              "PurplePink"
              "BrownYellow"
              "PurpleBrown"
              "Wisteria"
              "BananaYellow"
              "LipstickRed"
              "WaterBlue"
              "BrownGrey"
              "VibrantPurple"
              "BabyGreen"
              "BarfGreen"
              "EggshellBlue"
              "SandyYellow"
              "CoolGreen"
              "Pale"
              "BlueGrey"
              "HotMagenta"
              "Greyblue"
              "Purpley"
              "BabyShitGreen"
              "BrownishPink"
              "DarkAquamarine"
              "Diarrhea"
              "LightMustard"
              "PaleSkyBlue"
              "TurtleGreen"
              "BrightOlive"
              "DarkGreyBlue"
              "GreenyBrown"
              "LemonGreen"
              "LightPeriwinkle"
              "SeaweedGreen"
              "SunshineYellow"
              "UglyPurple"
              "MediumPink"
              "PukeBrown"
              "VeryLightPink"
              "Viridian"
              "Bile"
              "FadedYellow"
              "VeryPaleGreen"
              "VibrantGreen"
              "BrightLime"
              "Spearmint"
              "LightAquamarine"
              "LightSage"
              "Yellowgreen"
              "BabyPoo"
              "DarkSeafoam"
              "DeepTeal"
              "Heather"
              "RustOrange"
              "DirtyBlue"
              "FernGreen"
              "BrightLilac"
              "WeirdGreen"
              "PeacockBlue"
              "AvocadoGreen"
              "FadedOrange"
              "GrapePurple"
              "HotGreen"
              "LimeYellow"
              "Mango"
              "Shamrock"
              "Bubblegum"
              "PurplishBrown"
              "VomitYellow"
              "PaleCyan"
              "KeyLime"
              "TomatoRed"
              "Lightgreen"
              "Merlot"
              "NightBlue"
              "PurpleishPink"
              "Apple"
              "BabyPoopGreen"
              "GreenApple"
              "Heliotrope"
              "YellowGreen"
              "AlmostBlack"
              "CoolBlue"
              "LeafyGreen"
              "MustardBrown"
              "Dusk"
              "DullBrown"
              "FrogGreen"
              "VividGreen"
              "BrightLightGreen"
              "FluroGreen"
              "Kiwi"
              "Seaweed"
              "NavyGreen"
              "UltramarineBlue"
              "Iris"
              "PastelOrange"
              "YellowishOrange"
              "Perrywinkle"
              "Tealish"
              "DarkPlum"
              "Pear"
              "PinkishOrange"
              "MidnightPurple"
              "LightPurple"
              "DarkMint"
              "GreenishTan"
              "LightBurgundy"
              "TurquoiseBlue"
              "UglyPink"
              "Sandy"
              "ElectricPink"
              "MutedPurple"
              "MidGreen"
              "Greyish"
              "NeonYellow"
              "Banana"
              "CarnationPink"
              "Tomato"
              "Sea"
              "MuddyBrown"
              "TurquoiseGreen"
              "Buff"
              "Fawn"
              "MutedBlue"
              "PaleRose"
              "DarkMintGreen"
              "Amethyst"
              "BlueGreen"
              "Chestnut"
              "SickGreen"
              "Pea"
              "RustyOrange"
              "Stone"
              "RoseRed"
              "PaleAqua"
              "DeepOrange"
              "Earth"
              "MossyGreen"
              "GrassyGreen"
              "PaleLimeGreen"
              "LightGreyBlue"
              "PaleGrey"
              "Asparagus"
              "Blueberry"
              "PurpleRed"
              "PaleLime"
              "GreenishTeal"
              "Caramel"
              "DeepMagenta"
              "LightPeach"
              "MilkChocolate"
              "Ocher"
              "OffGreen"
              "PurplyPink"
              "Lightblue"
              "DuskyBlue"
              "Golden"
              "LightBeige"
              "ButterYellow"
              "DuskyPurple"
              "FrenchBlue"
              "UglyYellow"
              "GreenyYellow"
              "OrangishRed"
              "ShamrockGreen"
              "OrangishBrown"
              "TreeGreen"
              "DeepViolet"
              "Gunmetal"
              "BluePurple"
              "Cherry"
              "SandyBrown"
              "WarmGrey"
              "DarkIndigo"
              "Midnight"
              "BlueyGreen"
              "GreyPink"
              "SoftPurple"
              "Blood"
              "BrownRed"
              "MediumGrey"
              "Berry"
              "Poo"
              "PurpleyPink"
              "LightSalmon"
              "Snot"
              "EasterPurple"
              "LightYellowGreen"
              "DarkNavyBlue"
              "Drab"
              "LightRose"
              "Rouge"
              "PurplishRed"
              "SlimeGreen"
              "BabyPoop"
              "IrishGreen"
              "PinkPurple"
              "DarkNavy"
              "GreenyBlue"
              "LightPlum"
              "PinkishGrey"
              "DirtyOrange"
              "RustRed"
              "PaleLilac"
              "OrangeyRed"
              "PrimaryBlue"
              "KermitGreen"
              "BrownishPurple"
              "MurkyGreen"
              "Wheat"
              "VeryDarkPurple"
              "BottleGreen"
              "Watermelon"
              "DeepSkyBlue"
              "FireEngineRed"
              "YellowOchre"
              "PumpkinOrange"
              "PaleOlive"
              "LightLilac"
              "LightishGreen"
              "CarolinaBlue"
              "Mulberry"
              "ShockingPink"
              "Auburn"
              "BrightLimeGreen"
              "Celadon"
              "PinkishBrown"
              "PooBrown"
              "BrightSkyBlue"
              "Celery"
              "DirtBrown"
              "Strawberry"
              "DarkLime"
              "Copper"
              "MediumBrown"
              "MutedGreen"
              "Robin'sEgg"
              "BrightAqua"
              "BrightLavender"
              "Ivory"
              "VeryLightPurple"
              "LightNavy"
              "PinkRed"
              "OliveBrown"
              "PoopBrown"
              "MustardGreen"
              "OceanGreen"
              "VeryDarkBlue"
              "DustyGreen"
              "LightNavyBlue"
              "MintyGreen"
              "Adobe"
              "Barney"
              "JadeGreen"
              "BrightLightBlue"
              "LightLime"
              "DarkKhaki"
              "OrangeYellow"
              "Ocre"
              "Maize"
              "FadedPink"
              "BritishRacingGreen"
              "Sandstone"
              "MudBrown"
              "LightSeaGreen"
              "RobinEggBlue"
              "AquaMarine"
              "DarkSeaGreen"
              "SoftPink"
              "OrangeyBrown"
              "CherryRed"
              "BurntYellow"
              "BrownishGrey"
              "Camel"
              "PurplishGrey"
              "Marine"
              "GreyishPink"
              "PaleTurquoise"
              "PastelYellow"
              "BlueyPurple"
              "CanaryYellow"
              "FadedRed"
              "Sepia"
              "Coffee"
              "BrightMagenta"
              "Mocha"
              "Ecru"
              "Purpleish"
              "Cranberry"
              "DarkishGreen"
              "BrownOrange"
              "DuskyRose"
              "Melon"
              "SicklyGreen"
              "Silver"
              "PurplyBlue"
              "PurpleishBlue"
              "HospitalGreen"
              "ShitBrown"
              "MidBlue"
              "Amber"
              "EasterGreen"
              "SoftBlue"
              "CeruleanBlue"
              "GoldenBrown"
              "BrightTurquoise"
              "RedPink"
              "RedPurple"
              "GreyishBrown"
              "Vermillion"
              "Russet"
              "SteelGrey"
              "LighterPurple"
              "BrightViolet"
              "PrussianBlue"
              "SlateGreen"
              "DirtyPink"
              "DarkBlueGreen"
              "Pine"
              "YellowyGreen"
              "DarkGold"
              "Bluish"
              "DarkishBlue"
              "DullRed"
              "PinkyRed"
              "Bronze"
              "PaleTeal"
              "MilitaryGreen"
              "BarbiePink"
              "BubblegumPink"
              "PeaSoupGreen"
              "DarkMustard"
              "Shit"
              "MediumPurple"
              "VeryDarkGreen"
              "Dirt"
              "DuskyPink"
              "RedViolet"
              "LemonYellow"
              "Pistachio"
              "DullYellow"
              "DarkLimeGreen"
              "DenimBlue"
              "TealBlue"
              "LightishBlue"
              "PurpleyBlue"
              "LightIndigo"
              "SwampGreen"
              "BrownGreen"
              "DarkMaroon"
              "HotPurple"
              "DarkForestGreen"
              "FadedBlue"
              "DrabGreen"
              "LightLimeGreen"
              "SnotGreen"
              "Yellowish"
              "LightBlueGreen"
              "Bordeaux"
              "LightMauve"
              "Ocean"
              "Marigold"
              "MuddyGreen"
              "DullOrange"
              "Steel"
              "ElectricPurple"
              "FluorescentGreen"
              "YellowishBrown"
              "Blush"
              "SoftGreen"
              "BrightOrange"
              "Lemon"
              "PurpleGrey"
              "AcidGreen"
              "PaleLavender"
              "VioletBlue"
              "LightForestGreen"
              "BurntRed"
              "KhakiGreen"
              "Cerise"
              "FadedPurple"
              "Apricot"
              "DarkOliveGreen"
              "GreyBrown"
              "GreenGrey"
              "TrueBlue"
              "PaleViolet"
              "PeriwinkleBlue"
              "LightSkyBlue"
              "Blurple"
              "GreenBrown"
              "Bluegreen"
              "BrightTeal"
              "BrownishYellow"
              "PeaSoup"
              "Forest"
              "BarneyPurple"
              "Ultramarine"
              "Purplish"
              "PukeYellow"
              "BluishGrey"
              "DarkPeriwinkle"
              "DarkLilac"
              "Reddish"
              "LightMaroon"
              "DustyPurple"
              "TerraCotta"
              "Avocado"
              "MarineBlue"
              "TealGreen"
              "SlateGrey"
              "LighterGreen"
              "ElectricGreen"
              "DustyBlue"
              "GoldenYellow"
              "BrightYellow"
              "LightLavender"
              "Umber"
              "Poop"
              "DarkPeach"
              "JungleGreen"
              "Eggshell"
              "Denim"
              "YellowBrown"
              "DullPurple"
              "ChocolateBrown"
              "WineRed"
              "NeonBlue"
              "DirtyGreen"
              "LightTan"
              "IceBlue"
              "CadetBlue"
              "DarkMauve"
              "VeryLightBlue"
              "GreyPurple"
              "PastelPink"
              "VeryLightGreen"
              "DarkSkyBlue"
              "Evergreen"
              "DullPink"
              "Aubergine"
              "Mahogany"
              "ReddishOrange"
              "DeepGreen"
              "VomitGreen"
              "PurplePink"
              "DustyPink"
              "FadedGreen"
              "CamoGreen"
              "PinkyPurple"
              "PinkPurple"
              "BrownishRed"
              "DarkRose"
              "Mud"
              "Brownish"
              "EmeraldGreen"
              "PaleBrown"
              "DullBlue"
              "BurntUmber"
              "MediumGreen"
              "Clay"
              "LightAqua"
              "LightOliveGreen"
              "BrownishOrange"
              "DarkAqua"
              "PurplishPink"
              "DarkSalmon"
              "GreenishGrey"
              "Jade"
              "UglyGreen"
              "DarkBeige"
              "Emerald"
              "PaleRed"
              "LightMagenta"
              "Sky"
              "LightCyan"
              "YellowOrange"
              "ReddishPurple"
              "ReddishPink"
              "Orchid"
              "DirtyYellow"
              "OrangeRed"
              "DeepRed"
              "OrangeBrown"
              "CobaltBlue"
              "NeonPink"
              "RosePink"
              "GreyishPurple"
              "Raspberry"
              "AquaGreen"
              "SalmonPink"
              "Tangerine"
              "BrownishGreen"
              "RedBrown"
              "GreenishBrown"
              "Pumpkin"
              "PineGreen"
              "Charcoal"
              "BabyPink"
              "Cornflower"
              "BlueViolet"
              "Chocolate"
              "GreyishGreen"
              "Scarlet"
              "GreenYellow"
              "DarkOlive"
              "Sienna"
              "PastelPurple"
              "Terracotta"
              "AquaBlue"
              "SageGreen"
              "BloodRed"
              "DeepPink"
              "Grass"
              "Moss"
              "PastelBlue"
              "BluishGreen"
              "GreenBlue"
              "DarkTan"
              "GreenishBlue"
              "PaleOrange"
              "Vomit"
              "ForrestGreen"
              "DarkLavender"
              "DarkViolet"
              "PurpleBlue"
              "DarkCyan"
              "OliveDrab"
              "Pinkish"
              "Cobalt"
              "NeonPurple"
              "LightTurquoise"
              "AppleGreen"
              "DullGreen"
              "Wine"
              "PowderBlue"
              "OffWhite"
              "ElectricBlue"
              "DarkTurquoise"
              "BluePurple"
              "Azure"
              "BrightRed"
              "PinkishRed"
              "CornflowerBlue"
              "LightOlive"
              "Grape"
              "GreyishBlue"
              "PurplishBlue"
              "YellowishGreen"
              "GreenishYellow"
              "MediumBlue"
              "DustyRose"
              "LightViolet"
              "MidnightBlue"
              "BluishPurple"
              "RedOrange"
              "DarkMagenta"
              "Greenish"
              "OceanBlue"
              "Coral"
              "Cream"
              "ReddishBrown"
              "BurntSienna"
              "Brick"
              "Sage"
              "GreyGreen"
              "White"
              "Robin'sEggBlue"
              "MossGreen"
              "SteelBlue"
              "Eggplant"
              "LightYellow"
              "LeafGreen"
              "LightGrey"
              "Puke"
              "PinkishPurple"
              "SeaBlue"
              "PalePurple"
              "SlateBlue"
              "BlueGrey"
              "HunterGreen"
              "Fuchsia"
              "Crimson"
              "PaleYellow"
              "Ochre"
              "MustardYellow"
              "LightRed"
              "Cerulean"
              "PalePink"
              "DeepBlue"
              "Rust"
              "LightTeal"
              "Slate"
              "Goldenrod"
              "DarkYellow"
              "DarkGrey"
              "ArmyGreen"
              "GreyBlue"
              "Seafoam"
              "Puce"
              "SpringGreen"
              "DarkOrange"
              "Sand"
              "PastelGreen"
              "Mint"
              "LightOrange"
              "BrightPink"
              "Chartreuse"
              "DeepPurple"
              "DarkBrown"
              "Taupe"
              "PeaGreen"
              "PukeGreen"
              "KellyGreen"
              "SeafoamGreen"
              "BlueGreen"
              "Khaki"
              "Burgundy"
              "DarkTeal"
              "BrickRed"
              "RoyalPurple"
              "Plum"
              "MintGreen"
              "Gold"
              "BabyBlue"
              "YellowGreen"
              "BrightPurple"
              "DarkRed"
              "PaleBlue"
              "GrassGreen"
              "Navy"
              "Aquamarine"
              "BurntOrange"
              "NeonGreen"
              "BrightBlue"
              "Rose"
              "LightPink"
              "Mustard"
              "Indigo"
              "Lime"
              "SeaGreen"
              "Periwinkle"
              "DarkPink"
              "OliveGreen"
              "Peach"
              "PaleGreen"
              "LightBrown"
              "HotPink"
              "Black"
              "Lilac"
              "NavyBlue"
              "RoyalBlue"
              "Beige"
              "Salmon"
              "Olive"
              "Maroon"
              "BrightGreen"
              "DarkPurple"
              "Mauve"
              "ForestGreen"
              "Aqua"
              "Cyan"
              "Tan"
              "DarkBlue"
              "Lavender"
              "Turquoise"
              "DarkGreen"
              "Violet"
              "LightPurple"
              "LimeGreen"
              "Grey"
              "SkyBlue"
              "Yellow"
              "Magenta"
              "LightGreen"
              "Orange"
              "Teal"
              "LightBlue"
              "Red"
              "Brown"
              "Pink"
              "Blue"
              "Green"
              "Purple")))
  "List of colors provided by xkcdcolors package.")

(TeX-add-style-hook
 "xkcdcolors"
 (lambda ()
   ;; Run the style hook for xcolor.sty:
   (TeX-run-style-hooks "xcolor")
   ;; Make the colors defined in xkcdcolors.sty available:
   (apply #'LaTeX-add-xcolor-definecolors LaTeX-xkcdcolors-colornames))
 TeX-dialect)

(defvar LaTeX-xkcdcolors-package-options nil
  "Package options for the xkcdcolors package.")

;;; xkcdcolors.el ends here
