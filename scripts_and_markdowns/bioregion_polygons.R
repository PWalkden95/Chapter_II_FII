## SCRIPT -- Global ecoregions


## Development: Patrick Alexander Walkden

## Descirption: This script is to combine the 847 WWF ecoregions into the 52 bioregions as described by oneearth -- https://www.oneearth.org/bioregions/

rm(list = ls())

require(terra)
require(rgdal)
require(magrittr)
require(tidyverse)
require(raster)
require(sf)

## equal are projection crs

newcrs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"


## load in the ecoregions shapefles downloaded from ....

ecoregions <- st_read("outputs/Ecoregions2017/Ecoregions2017.shp")

ecoregions$ECO_NAME[order(ecoregions$ECO_NAME)]

## want to be able to visulise the location of the ecoregions so a quick function to do that 

bioregions_list <- list(
                        `Brazil Atlantic Dry Forest` = c("Brazilian Atlantic Dry Forests","Caatinga","Caatinga Enclaves Moist Forests","Maranhão Babaçu forests"),
                        `Cerrado Savannas` = c("Cerrado", "Campos Rupestres Montane Savanna"),
                        `Pantanal Flooded Grasslands & Dry Forests` = c("Chiquitano Dry Forests","Pantanal"),
                        `Brazilian Atlantic Moist Forests` = c("Alto Paraná Atlantic Forests","Araucaria Moist Forests","Atlantic Coast Restingas","Serra Do Mar Coastal Forests","Bahia Interior Forests","Bahia Coastal Forests","Trindade-Martin Vaz Islands Tropical Forests","Pernambuco Interior Forests","Pernambuco Coastal Forests","Southern Atlantic Brazilian Mangroves","Fernando De Noronha-Atol Das Rocas Moist Forests","St. Peter and St. Paul Rocks"),
                        `Andean Mountain Forests & Valleys` = c("Southern Andean Yungas","Bolivian Montane Dry Forests","Bolivian Yungas","Peruvian Yungas","Cordillera Central Páramo","Marañón Dry Forests","Eastern Cordillera Real Montane Forests","Northern Andean Páramo","Patía valley dry forests","Northwest Andean Montane Forests","Magdalena Valley Montane Forests","Magdalena Valley Dry Forests","Cauca Valley Dry Forests","Cauca Valley Montane Forests","Cordillera Oriental Montane Forests","Cordillera De Merida Páramo","Venezuelan Andes Montane Forests"),
                        `Ecuadorean Dry Coastal Forests & Flooded Grasslands` = c("Western Ecuador Moist Forests","Ecuadorian Dry Forests","Guayaquil Flooded Grasslands","Tumbes-Piura Dry Forests","South American Pacific Mangroves"),
                        `South American Coastal Deserts` = c("Sechura Desert","Atacama Desert"),
                        `Juan Fernández & Desventuradas Islands` = c("Juan Fernández Islands Temperate Forests", "San Félix-San Ambrosio Islands Temperate Forests"),
                        `Chilean Matorral Shrublands & Savanna` = c("Chilean Matorral"),
                        `Andean Mountain Grasslands` = c("Southern Andean Steppe","High Monte","Central Andean Puna","Central Andean Dry Puna","Central Andean Wet Puna"),
                        `Galápagos Islands` = c("Galápagos Islands Xeric Scrub"),
                        `Chilean Mixed Forests` = c("Magellanic Subpolar Forests","Valdivian Temperate Forests"),
                        `Chaco Grasslands` = c("Dry Chaco","Humid Chaco"),
                        `Rio de la Plata Grasslands` = c("Espinal","Humid Pampas","Paraná Flooded Savanna","Southern Cone Mesopotamian Savanna","Uruguayan Savanna"),
                        `Patagonia Steppe & Low Mountains` = c("Patagonian Steppe","Low Monte"),
                        `Northern Amazonian Forests` = c("Negro-Branco Moist Forests","Japurá-Solimões-Negro Moist Forests","Rio Negro Campinarana","Uatumã-Trombetas Moist Forests","Gurupa Várzea"),
                        `Western Amazonian Forests & Plains` = c("Ucayali Moist Forests","Iquitos Várzea","Southwest Amazon Moist Forests","Beni Savanna"),
                        `Southern Amazonian Forests` = c("Madeira-Tapajós Moist Forests","Mato Grosso Tropical Dry Forests","Tapajós-Xingu Moist Forests","Xingu-Tocantins-Araguaia Moist Forests"),
                        `Amazon River Estuary` = c("Marajó Várzea","Tocantins/Pindare Moist Forests","Northeast Brazil Restingas","Amazon-Orinoco-Southern Caribbean Mangroves"),
                        `Central Amazonian Forests` = c("Napo Moist Forests","Caqueta Moist Forests","Solimões-Japurá Moist Forests","Purus várzea","Juruá-Purus Moist Forests","Monte Alegre Várzea","Purus-Madeira Moist Forests"),
                        `Venezuelan Coast` = c("Guajira-Barranquilla Xeric Scrub", "Araya and Paria Xeric Scrub","Sinú Valley Dry Forests","Santa Marta Montane Forests","Santa Marta Páramo","Maracaibo Dry Forests","Catatumbo Moist Forests","Paraguaná xeric scrub","Lara-Falcón Dry Forests","Cordillera La Costa montane forests","La Costa Xeric Shrublands","Amazon-Orinoco-Southern Caribbean Mangroves"),
                        `Llanos & Dry Forests` = c("Apure-Villavicencio Dry Forests","Llanos"),
                        `Guianan Forests & Savanna` = c("Guianan Savanna","Pantepui forests & shrublands","Guianan Highlands Moist Forests","Guianan Piedmont Moist Forests","Orinoco Delta Swamp Forests","Orinoco Wetlands","Trinidad and Tobago dry forest","Trinidad and Tobago Moist Forest","Guianan Lowland Moist Forests","Guianan Freshwater Swamp Forests","Amazon-Orinoco-Southern Caribbean Mangroves"),
                        `Far Northern Pacific Coast` = c("Pacific Coastal Mountain Icefields and Tundra","Northern Pacific Alaskan Coastal Forests"),
                        `Greater Alaska Taiga & Tundra` = c("Aleutian Islands Tundra","Beringia Lowland Tundra","Alaska Peninsula Montane Taiga","Alaska-St. Elias Range Tundra","Cook Inlet Taiga","Copper Plateau Taiga","Interior Yukon-Alaska alpine tundra","Interior Alaska-Yukon Lowland Taiga","Beringia Upland Tundra","Ahklun and Kilbuck Upland Tundra"),
                        `North Alaskan Tundra` = c("Arctic Coastal Tundra","Arctic Foothills Tundra","Brooks-British Range Tundra"),
                        `Canadian Tundra` = c("Canadian Low Arctic Tundra","Canadian Middle Arctic Tundra","Canadian High Arctic Tundra","Davis Highlands Tundra","Torngat Mountain Tundra"),
                        `Canadian Shield & Coastal Taiga-Forests` = c("Northern Canadian Shield Taiga","Midwest Canadian Shield Forests","Southern Hudson Bay Taiga","Central Canadian Shield Forests","Eastern Canadian Shield Taiga","Eastern Canadian forests"),
                        `Mid-Canada Boreal Plains & Foothill Forests` = c("Alberta-British Columbia Foothills Forests","Mid-Canada Boreal Plains Forests"),
                        `Northwest Canadian Taiga, Lakes, & Wetlands` = c("Northwest Territories Taiga","Muskwa-Slave Lake Taiga"),
                        `Greater Yukon` = c("Ogilvie-MacKenzie Alpine Tundra","Watson Highlands Taiga","Northern Cordillera Forests"),
                        `Greenland` = c("Kalaallit Nunaat Arctic Steppe","Kalaallit Nunaat High Arctic Tundra"),
                        `Midwestern Tallgrass Prairie & Forest Transition` = c("Flint Hills Tallgrass Prairie","Central Tallgrass Prairie","Central US Forest-Grasslands Transition"),
                        `Northern Prairie & Aspen Forests` = c("Northern Shortgrass Prairie","Canadian Aspen Forests and Parklands","Northern Tallgrass Prairie"),
                        `Southern Prairie Mixed Grasslands` = c("Western Shortgrass Prairie","Nebraska Sand Hills Mixed Grasslands","Central-Southern US Mixed Grasslands","Cross-Timbers Savanna-Woodland","Edwards Plateau Savanna"),
                        `Northwest Intermountain Conifer Forests` = c("Fraser Plateau and Basin Conifer Forests","Okanogan Dry Forests"),
                        `Greater Rockies & Mountain Forests` = c("Central British Columbia Mountain Forests","Northern Rockies Conifer Forests","South Central Rockies Forests","Montana Valley and Foothill Grasslands"),
                        `Colorado Plateau & Mountain Forests` = c("Wyoming Basin Shrub Steppe","Wasatch and Uinta Montane Forests","Colorado Plateau Shrublands","Colorado Rockies Forests","Arizona Mountains Forests"),
                        `Great Basin & Columbia Steppe` = c("Snake-Columbia Shrub Steppe","Great Basin Shrub Steppe","Great Basin Montane Forests"),
                        `Columbia Plateau & Blue Mountains` = c("Palouse Prairie","Blue Mountains Forests"),
                        `Greater California` = c("Santa Lucia Montane Chaparral & Woodlands","California Interior Chaparral and Woodlands","California Central Valley Grasslands","Sierra Nevada Forests","California Montane Chaparral and Woodlands","California Coastal Sage and Chaparral"),
                        `Cascades Mountain Forests & Valleys` = c("Willamette Valley Oak Savanna","Central-Southern Cascades Forests","Eastern Cascades Forests","North Cascades Conifer Forests"),
                        `Pacific Northwest Coastal Forests` = c("Queen Charlotte Islands Conifer Forests","British Columbia Coastal Conifer Forests","Central Pacific Northwest Coastal Forests","Puget Lowland Forests","Klamath-Siskiyou Forests","Northern California Coastal Forests"),
                        `Interior Plateau & Southern Great Lakes Forests` = c("Southern Great Lakes Forests","Interior Plateau US Hardwood Forests"),
                        `Appalachia & Allegheny Interior Forests` = c("Allegheny Highlands Forests","Appalachian Mixed Mesophytic Forests","Appalachian-Blue Ridge Forests","Appalachian Piedmont Forests"),
                        `Northern Great Lakes Forests` = c("Western Great Lakes Forests","Upper Midwest US Forest-Savanna Transition","Eastern Canadian Forest-Boreal transition","Eastern Great Lakes Lowland Forests"),
                        `Northeastern American Mixed Forests` = c("Northeast US Coastal Forests","New England-Acadian Forests","Gulf of St. Lawrence Lowland Forests"),
                        `Southern Mixed Forests & Blackland Prairies` = c("Texas Blackland Prairies", "East Central Texas forests", "Piney Woods"),
                        `Western Gulf Coastal Grasslands` = c("Western Gulf Coastal Grasslands"),
                        `Bermuda` = c("Bermuda Subtropical Conifer Forests"),
                        `Southeast Savannas & Riparian Forests` = c("Mississippi Lowland Forests","Southeast US Conifer Savannas","Mid-Atlantic US Coastal Savannas","Atlantic Coastal Pine Barrens"),
                        `Ozarks Mixed Forests` = c("Ozark Mountain Forests","Ozark Highlands Mixed Forests"),
                        `Baja California & Southern Deserts` = c("Mojave Desert","Sonoran Desert","Baja California Desert","Gulf of California Xeric Scrub"),
                        `Sierra Madre Forests & Mexican Drylands` = c("Sonoran-Sinaloan Subtropical Dry Forest","Sierra Madre Occidental Pine-Oak Forests","Chihuahuan Desert","Tamaulipan Mezquital","Sierra Madre Oriental Pine-Oak Forests","Tamaulipan Matorral","Meseta Central Matorral","Central Mexican Matorral"),
                        `Central American Isthmian & Colombian Coastal Forests` = c("Isthmian-Atlantic Moist Forests","Southern Mesoamerican Pacific Mangroves","Talamancan Montane Forests","Isthmian-Pacific Moist Forests","Panamanian Dry Forests","Malpelo Island Xeric Scrub","Magdalena-Urabá Moist Forests","Chocó-Darién Moist Forests","South American Pacific Mangroves","Eastern Panamanian Montane Forests","Cocos Island Moist Forests"),
                        `Yucatan & Veracruz Mixed Forests` = c("Veracruz Moist Forests","Veracruz Montane Forests","Veracruz Dry Forests","Sierra De Los Tuxtlas","Petén-Veracruz Moist Forests","Pantanos De Centla","Yucatán dry forests","Yucatán Moist Forests","Belizian pine savannas","Mesoamerican Gulf-Caribbean Mangroves"),
                        `Mexican Subtropical Islands` = c("Islas Revillagigedo Dry Forests","Clipperton Island Shrub and Grasslands"),
                        `Mexican Dry & Coniferous Forests` = c("San Lucan Xeric Scrub","Sierra de la Laguna Pine-Oak Forests","Sierra de la Laguna Dry Forests","Sinaloan Dry Forests","Northern Mesoamerican Pacific Mangroves","Trans-Mexican Volcanic Belt Pine-Oak Forests","Jalisco Dry Forests","Bajío Dry Forests","Balsas Dry Forests","Sierra Madre Del Sur Pine-Oak Forests","Tehuacán Valley Matorral","Oaxacan Montane Forests","Sierra Madre De Oaxaca Pine-Oak Forests","Southern Pacific Dry Forests"),
                        `Central American Mixed Forests` = c("Southern Mesoamerican Pacific Mangroves","Central American Dry Forests","Chimalapas Montane Forests","Sierra Madre De Chiapas Moist Forests","Chiapas Depression Dry Forests","Central American Pine-Oak Forests","Chiapas Montane Forests","Costa Rican Seasonal Moist Forests","Motagua Valley Thornscrub","Central American Atlantic Moist Forests","Central American Montane Forests","Miskito Pine Forests","Mesoamerican Gulf-Caribbean Mangroves","Cayos Miskitos-San Andrés and Providencia Moist Forests"),
                        `Caribbean Islands` = c("Everglades Flooded Grasslands","Bahamian-Antillean Mangroves","Bahamian Pineyards","Cuban Pine Forests","Cuban Moist Forests","Cuban Cactus Scrub","Cuban Dry Forests","Cuban Wetlands","Jamaican Moist Forests","Jamaican Dry Forests","Hispaniolan Dry Forests","Hispaniolan Moist Forests","Hispaniolan Pine Forests","Enriquillo Wetlands","Puerto Rican Moist Forests","Puerto Rican Dry Forests","Leeward Islands Moist Forests","Caribbean Shrublands","Lesser Antillean Dry Forests","Windward Islands Moist Forests"),
                        `Southwest African Coastal Drylands` = c("Namibian Savanna Woodlands","Kaokoveld Desert","Namib Desert","Namaqualand-Richtersveld Steppe"),
                        `Greater Karoo & Kalahari Drylands` = c("Kalahari Acacia Woodlands","Kalahari Xeric Savanna","Gariep Karoo","Nama Karoo Shrublands"),
                        `Southeast African Subtropical Grasslands` = c("Central Bushveld","Limpopo Lowveld","Highveld Grasslands","Drakensberg Grasslands","Drakensberg Escarpment Savanna and Thicket"),
                        `South African Cape Shrublands & Mountain Forests` = c("Renosterveld Shrubland","Fynbos Shrubland","Succulent Karoo Xeric Shrublands","Knysna-Amatole Montane Forests","Albany Thickets"),
                        `Tristan Volcanic Islands` = c("Tristan Da Cunha-Gough Islands Shrub and Grasslands"),
                        `West African Coastal Forests & Savanna` = c("Guinean Forest-Savanna","Guinean Montane Forests","Western Guinean Lowland Forests","Eastern Guinean Forests","Jos Plateau Forest-Grassland","Guinean Mangroves","Central African Mangrove"),
                        `Gulf of Guinea Coastal Forests & Mangroves` = c("Nigerian Lowland Forests","Niger Delta Swamp Forests","Cross-Niger Transition Forests","Cross-Sanaga-Bioko Coastal Forests","Congolian Coastal Forests","Mount Cameroon and Bioko Montane Forests","São Tomé, Príncipe, and Annobón Forests","Central African Mangroves"),
                        `Mandara Mountain & North Congolian Forest-Savannas` = c("Cameroon Highlands Forests", "Northern Congolian Forest-Savanna"),
                        `North Congolian Lowland Forests` = c("Northwest Congolian Lowland Forests","Northeast Congolian lowland forests"),
                        `Central Congolian Tropical Forests` = c("Western Congolian Swamp Forests","Eastern Congolian Swamp Forests","Central Congolian Lowland Forests"),
                        `South Congolian Forest-Savannas & Coastal Scarp` = c("Western Congolian Forest-Savanna", "Southern Congolian Forest-Savanna"),
                        `Victoria Basin & Albertine Rift Forests` = c("Albertine Rift Montane Forests", "Victoria Basin Forest-Savanna","Rwenzori-Virunga Montane Moorlands"),
                        `St. Helena & Ascension Islands` = c("St. Helena Scrub and Woodlands","Ascension Scrub and Grasslands"),
                        `Greater African Subequatorial Savannas & Mixed Woodlands` = c("Angolan Mopane Woodlands","Etosha Pan Halophytics","Zambezian Baikiaea Woodlands","Angolan Montane Forest-Grassland","Angolan Wet Miombo Woodlands","Dry Miombo Woodlands","Zambezian Flooded Grasslands","Zambezian Evergreen Dry Forests","Central Zambezian Wet Miombo Woodlands","Zambezian-Limpopo Mixed Woodlands","Makgadikgadi Halophytics","Zambezian Mopane Woodlands","Nyanga-Chimanimani Montane Forest-Grassland","Itigi-Sumbu Thicket","Southern Rift Montane Forest-Grassland","Eastern Arc Forests","Southern Acacia-Commiphora Bushlands and Thickets","East African Halophytics","Mulanje Montane Forest-Grassland","Angolan Scarp Savanna and Woodlands"),
                        `Mascarene Tropical Forest Islands` = c("Mascarene Forests"),
                        `Amsterdam-Saint Paul Islands` = c("Amsterdam-Saint Paul Islands Temperate Grasslands"),
                        `East African Coastal Forests` = c("Northern Swahili Coastal Forests","East African Mangroves","Southern Swahili Coastal Forests and Woodlands","Zambezian Coastal Flooded Savanna","Maputaland Coastal Forests and Woodlands","Southern Africa Mangroves","Kwazulu Natal-Cape Coastal Forests"),
                        `Madagascar Island` = c("Madagascar Subhumid Forests","Madagascar Humid Forests","Madagascar Succulent Woodlands","Madagascar Spiny Thickets","Madagascar Mangroves","Madagascar Ericoid Thickets","Ile Europa and Bassas Da India Xeric Scrub"),
                        `Seychelles & Comoros Tropical Islands` = c("Comoros Forests","Aldabra Island Xeric Scrub","Granitic Seychelles Forests"),
                        `Sahel Acacia Savannas` = c("Inner Niger Delta Flooded Savanna","Sahelian Acacia Savanna","Lake Chad Flooded Savanna"),
                        `South Red Sea & Gulf of Alden Coastal Drylands` = c("Red Sea Mangroves","Djibouti Xeric Shrublands","Eritrean Coastal Desert","Horn of Africa Xeric Bushlands","Somali Montane Xeric Woodlands","Hobyo Grasslands and Shrublands","Socotra Island Xeric Shrublands","South Arabian Fog Woodlands, Shrublands, and Dune","Southwest Arabian Escarpment Shrublands and Woodlands","Southwest Arabian Coastal Xeric Shrublands","Southwest Arabian Montane Woodlands and Grasslands","Southwest Arabian Highland Xeric Scrub"),
                        `Lake Turkana-Sudd Grasslands, Bushlands & Forests` = c("Mandara Plateau Woodlands","East Sudanian Savanna","Sudd Flooded Grasslands","East African Montane Forests","Ethiopian Montane Grasslands and Woodlands","Ethiopian Montane Moorlands","Ethiopian Montane Forests","Northern Acacia-Commiphora Bushlands and Thickets","Masai Xeric Grasslands and Shrublands","Somali Acacia-Commiphora Bushlands and Thickets","East African Montane Moorlands"),
                        `West Sudanian Savanna` = c("West Sudanian Savanna"),
                        `Cape Verde Islands` = c("Cape Verde Islands Dry Forests"),
                        `Ural Mountains & West Eurasian Taiga Forests` = c("Scandinavian and Russian Taiga","Urals Montane Forest and Taiga"),
                        `Scandinavian Birch & Coastal Conifer Forests` = c("Scandinavian Coastal Conifer Forests","Scandinavian Montane Birch Forest and Grasslands"),
                        `Iceland` = c("Boreal Birch Forests and Alpine Tundra"),
                        `Greater Eurasian Tundra` = c("Kola Peninsula Tundra","Northwest Russian-Novaya Zemlya Tundra","Yamal-Gydan Tundra","Taimyr-Central Siberian Tundra","Novosibirsk Islands Arctic Desert","Northeast Siberian Coastal Tundra"),
                        `Russian Arctic Desert Islands` = c("Russian Arctic desert"),
                        `Sea of Okhotsk Coastal Taiga, Meadows & Tundra` = c("Okhotsk-Manchurian Taiga","Sakhalin Island Taiga","Kamchatka-Kurile Meadows and Sparse Forests","Kamchatka Tundra","Kamchatka Taiga"),
                        `East Eurasian Coastal Tundra` = c("Russian Bering Tundra","Chukchi Peninsula Tundra","Wrangel Island Arctic Desert"),
                        `Siberian Boreal Forests & Mountain Tundra` = c("West Siberian Taiga","East Siberian Taiga","Cherskii-Kolyma Mountain Tundra","Trans-Baikal Bald Mountain Tundra","Cherskii-Kolyma Mountain Tundra","Northeast Siberian Taiga"),
                        `Greater Gobi Desert` = c("Alashan Plateau Semi-Desert","Gobi Lakes Valley Desert Steppe","Eastern Gobi Desert Steppe"),
                        `Taklimakan Desert & Lowland Deciduous Forests` = c("Tarim Basin Deciduous Forests and Steppe","Taklimakan Desert"),
                        `Junggar Semi-Desert & Ermin Valley Steppe` = c("Emin Valley Steppe","Junggar Basin Semi-Desert"),
                        `Tibetan-Pamir Alpine Steppes, Shrublands & Mountain Forests` = c("Karakoram-West Tibetan Plateau Alpine Steppe","Yarlung Zanbo Arid Steppe","Western Himalayan Alpine Shrub and Meadows","Pamir Alpine Desert and Tundra","Northwestern Himalayan Alpine Shrub and Meadows","Hindu Kush Alpine Meadow","Eastern Himalayan Alpine Shrub and Meadows","Northeast Himalayan subalpine conifer forests"),
                        `Greater Tibetan Plateau Alpine Meadows & Shrublands` = c("Qilian Mountains Conifer Forests","entral Tibetan Plateau Alpine Steppe","North Tibetan Plateau-Kunlun Mountains Alpine Desert","Qilian Mountains Subalpine Meadows","Southeast Tibet Shrublands and Meadows","Tibetan Plateau Alpine Shrublands and Meadows","Qaidam Basin Semi-Desert"),
                        `Mongolian Grasslands, Alpine Meadows & Forest Steppe` = c("Khangai Mountains Conifer Forests","Daurian Forest Steppe","Mongolian-Manchurian Grassland","Selenge-Orkhon Forest Steppe","Nenjiang River Grassland","Khangai Mountains Alpine Meadow"),
                        `Ordos Plateau Steppe & Mountain Conifer Forests` = c("Helanshan Montane Conifer Forests","Ordos Plateau Steppe"),
                        `Korean Peninsula Mixed Forests` = c("Central Korean Deciduous Forests","Southern Korea Evergreen Forests"),
                        `Manchuria-Ussuri Mixed Forests & Meadow Steppes` = c("Northeast China Plain Deciduous Forests","Changbai Mountains Mixed Forests","Manchurian Mixed Forests","Suiphun-Khanka Meadows and Forest Meadows","Amur Meadow Steppe","Ussuri Broadleaf and Mixed Forests"),
                        `Dzhagdy Mountain Conifer Forests` = c("Da Hinggan-Dzhagdy Mountains Conifer Forests"),
                        `Japan Forest Islands` = c("Hokkaido Deciduous Forests","Nihonkai Evergreen Forests","Nihonkai Montane Deciduous Forests","Taiheiyo Evergreen Forests","Taiheiyo Montane Deciduous Forests","Hokkaido Montane Conifer Forests","Honshu Alpine Conifer Forests"),
                        `Hengduan Mountain Conifer Forests` = c("Hengduan Mountains Subalpine Conifer Forests","Nujiang Langcang Gorge alpine conifer and mixed forests","Qionglai-Minshan Conifer Forests"),
                        `Guizhou & Yunnan Subtropical Forest Plateaus` = c("Yunnan Plateau Subtropical Evergreen Forests","Guizhou Plateau Broadleaf and Mixed Forests"),
                        `Sichuan Basin & Central Mountain Forests` = c("Daba Mountains Evergreen Forests","Qin Ling Mountains Deciduous Forests","Sichuan Basin Evergreen Broadleaf Forests"),
                        `Chang Jiang Plain Evergreen Forests` = c("Changjiang Plain Evergreen Forests","Yellow Sea Saline Meadow"),
                        `Loess Plateau & Huang He Plain Mixed Forests` = c("Central China Loess Plateau Mixed Forests","Huang He Plain Mixed Forests","Bohai Sea Saline Meadow"),
                        `Altai Mountain Forests, Grasslands & Desert Steppe` = c("Altai Montane Forest and Forest Steppe","Altai Steppe and Semi-Desert","Emin Valley Steppe","Altai Alpine Meadow and Tundra"),
                        `Sayan Mountain Conifer Forests, Alpine Meadows & Steppe` = c("Sayan Montane Conifer Forests","Sayan Intermontane Steppe","South Siberian Forest Steppe","Sayan Alpine Meadows and Tundra"),
                        `Afghan-Balochistan Drylands, Mountain Meadows & Conifer Forests` = c("Badghyz and Karabil Semi-Desert","Paropamisus Xeric Woodlands","Afghan Mountains Semi-Desert","Ghorat-Hazarajat Alpine Meadow","Central Afghan Mountains Xeric Woodlands","East Afghan Montane Conifer Forests","Sulaiman Range Alpine Meadows","Baluchistan xeric woodlands"),
                        `Persian Deserts & Mountain Woodlands` = c("Central Persian Desert Basins","Kuh Rud and Eastern Iran Montane Woodlands","Registan-North Pakistan Sandy Desert","South Iran Nubo-Sindian Desert and Semi-Desert"),
                        `South Caspian Coastal & Mountain Mixed Forests` = c("Caspian Hyrcanian Mixed Forests","Elburz Range Forest Steppe"),
                        `Zagros Mountain Forests & East Anatolian Steppe` = c("Eastern Anatolian Montane Steppe","Zagros Mountains Forest Steppe"),
                        `Tian Shan-Pamir Grasslands, Mountain Steppe & Conifer Forests` = c("Tian Shan Montane Conifer Forests","Alai-Western Tian Shan Steppe","Gissaro-Alai Open Woodlands","Tian Shan Foothill Arid Steppe","Tian Shan Montane Steppe and Meadows"),
                        `Caspian Sea, Coastal Deserts & Kopet Dagh Mountain Woodlands` = c("Kopet Dag Woodlands and Forest Steppe","Azerbaijan Shrub Desert and Steppe","Caspian Lowland Desert","Kopet Dag Semi-Desert"),
                        `Central Asian Deserts & Riparian Woodlands` = c("Central Asian Northern Desert","Central Asian Riparian Woodlands","Central Asian Southern Desert","Kazakh Semi-Desert"),
                        `Siberian Hemiboreal Forests & Steppe` = c("Western Siberian Hemiboreal Forests"),
                        `Kazakh Forest Steppe & Grasslands` = c("Kazakh Forest Steppe","Kazakh Steppe","Kazakh Upland Steppe"),
                        `Great Britain, Ireland & Faroe Islands` = c("Faroe Islands Boreal Grasslands","North Atlantic Moist Mixed Forests","Caledon Conifer Forests","Celtic Broadleaf Forests","English Lowlands Beech Forests"),
                        `European Interior Mixed Forests` = c("Western European Broadleaf Forests","Central European Mixed Forests","East European Forest Steppe"),
                        `Baltic Sea & Sarmatic Mixed Forests` = c("Sarmatic Mixed Forests"),
                        `West European Coastal Mixed Forests` = c("Baltic Mixed Forests","Cantabrian Mixed Forests","European Atlantic Mixed Forests","Pyrenees Conifer and Mixed Forests"),
                        `Dinaric Mountains & Balkan Mixed Forests` = c("Balkan Mixed Forests","Dinaric Mountains Mixed Forests","Rodope Montane Mixed Forests"),
                        `Alps & Po Basin Mixed Forests` = c("Po Basin Mixed Forests", "Alps Conifer and Mixed Forests"),
                        `Carpathian Mountain & Plains Mixed Forests` = c("Pannonian Mixed Forests","Carpathian Montane Forests"),
                        `Black Sea, Caucasus-Anatolian Mixed Forests & Steppe` = c("Caucasus Mixed Forests","Central Anatolian Steppe and Woodlands","Crimean Submediterranean Forest Complex","Eastern Anatolian Deciduous Forests","Euxine-Colchic Broadleaf Forests","Northern Anatolian Conifer and Deciduous Forests","Central Anatolian Steppe"),
                        `Pontic Steppe Grasslands` = c("Pontic Steppe"),
                        `Adriatic Sea & Central Mediterranean Mixed Forests` = c("Appenine deciduous montane forests","Corsican Montane Broadleaf and Mixed Forests","Illyrian Deciduous Forests","Italian Sclerophyllous and Semi-Deciduous Forests","South Apennine Mixed Montane Forests","Tyrrhenian-Adriatic Sclerophyllous and Mixed Forests"),
                        `Aegean Sea & East Mediterranean Mixed Forests` = c("Aegean and Western Turkey Sclerophyllous and Mixed Forests","Anatolian Conifer and Deciduous Mixed Forests","Crete Mediterranean Forests","Cyprus Mediterranean Forests","Eastern Mediterranean Conifer-Broadleaf Forests","Pindus Mountains Mixed Forests","Southern Anatolian Montane Conifer and Deciduous Forests"),
                        `Azores Forest Islands` = c("Azores Temperate Mixed Forests"),
                        `Balearic Sea & West Mediterranean Mixed Forests` = c("Iberian Conifer Forests","Iberian Sclerophyllous and Semi-Deciduous Forests","Northeast Spain and Southern France Mediterranean forests","Northwest Iberian Montane Forests","Southeast Iberian shrubs and woodlands","Southwest Iberian Mediterranean Sclerophyllous and Mixed Forests"),
                        `Northern Sahara Deserts, Savannas & Marshes` = c("Canary Islands Dry Woodlands and Forests","Saharan Atlantic Coastal Desert","North Saharan Xeric Steppe and Woodland","West Sahara Desert","East Sahara Desert","Saharan Halophytics","Nile Delta Flooded Savanna"),
                        `Southern Sahara Deserts & Mountain Woodlands` = c("East Saharan Montane Xeric Woodlands","South Sahara Desert","Tibesti-Jebel Uweinat Montane Xeric Woodlands","West Saharan Montane Xeric Woodlands"),
                        `South Mediterranean Mixed Woodlands & Forests` = c("Mediterranean Conifer and Mixed Forests","Mediterranean High Atlas Juniper Steppe","Mediterranean Acacia-Argania Dry Woodlands and Succulent Thickets","Mediterranean Dry Woodlands and Steppe","Mediterranean Woodlands and Forests"),
                        `Madeira Evergreen Island` = c("Madeira Evergreen Forests"),
                        `Red Sea, Arabian Deserts & Salt Marshes` = c("Al-Hajar Foothill Xeric Woodlands and Shrublands","Al-Hajar Montane Woodlands and Shrublands","Syrian Xeric Grasslands and Shrublands","Tigris-Euphrates Alluvial Salt Marsh","Arabian Desert","Arabian Sand Desert","Arabian-Persian Gulf Coastal Plain Desert","East Arabian Fog Shrublands and Sand Desert","Mesopotamian Shrub Desert","North Arabian Desert","North Arabian Highland Shrublands","Red Sea Coastal Desert","Red Sea-Arabian Desert Shrublands","South Arabian Plains and Plateau Desert"),
                        `Borneo Tropical Forests & Sundaland Heath Forests` = c("Borneo lowland rain forests","Borneo montane rain forests","Borneo Peat Swamp Forests","Southwest Borneo Freshwater Swamp Forests","Sundaland Heath Forests","Kinabalu Montane Alpine Meadows"),
                        `Peninsular Malaysian & Sumatran Tropical Rainforests` = c("Mentawai Islands rain forests","Sulawesi montane rain forests","Sumatran Lowland Rain forests","Sumatran Tropical Pine Forests","Sumatran Freshwater Swamp Forests","Sumatran Peat Swamp Forests","Peninsular Malaysian Rain forests","Peninsular Malaysian Peat Swamp Forests","Peninsular Malaysian Montane Rain forests","Sunda Shelf Mangroves"),
                        `Javan-Bali Tropical Rainforests` = c("Christmas and Cocos Islands Tropical Forests","Eastern Java-Bali Montane Rain forests","Eastern Java-Bali Rain forests","Western Java Montane Rain forests","Western Java Rain forests"),
                        `Greater Deccan-Sri Lankan Forests & Drylands` = c("Central Deccan Plateau Dry Deciduous Forests","Deccan Thorn Scrub Forests","South Deccan Plateau Dry Deciduous Forests","East Deccan Dry-Evergreen Forests","Sri Lanka Dry-Zone Dry Evergreen Forests","Godavari-Krishna Mangroves"),
                        `Northern Deccan & Odisha Tropical Forests` = c("East Deccan Moist Deciduous Forests","North Deccan Dry Deciduous Forests","Orissa Semi-Evergreen Forests","Godavari-Krishna Mangroves"),
                        `North Indian Tropical Forests & Sundarbans` = c("Upper Gangetic Plains Moist Deciduous Forests","Lower Gangetic Plains Moist Deciduous Forests","Meghalaya Subtropical Forests","Brahmaputra Valley Semi-Evergreen Forests","Sundarbans Freshwater Swamp Forests","Sundarbans Mangroves","Myanmar Coast Mangroves"),
                        `Himalayan Mixed Forests & Grasslands` = c("Eastern Himalayan Subalpine Conifer Forests","Eastern Himalayan Broadleaf Forests","Himalayan Subtropical Pine Forests","Terai-Duar Savanna and Grasslands","Himalayan Subtropical Broadleaf Forests","Western Himalayan Subalpine Conifer Forests","Western Himalayan Broadleaf Forests"),
                        `Indian Dry Deciduous Forests` = c("Chhota-Nagpur Dry Deciduous Forests","Narmada Valley Dry Deciduous Forests","Khathiar-Gir Dry Deciduous Forests","Indus River Delta-Arabian Sea Mangroves"),
                        `Indian Tropical Coastal Forests` = c("Sri Lanka Lowland Rain Forests","Sri Lanka Montane Rain Forests","Malabar Coast Moist Forests","South Western Ghats Montane Rain Forests","South Western Ghats Moist Deciduous Forests","North Western Ghats Montane Rain Forests","North Western Ghats Moist Deciduous Forests"),
                        `North Indomalayan Deserts & Scrub Forest` = c("Indus Valley Desert","Aravalli West Thorn Scrub Forests","Thar Desert","Rann of Kutch Seasonal Salt Marsh","Indus River Delta-Arabian Sea Mangroves"),
                        `Central Indian Ocean Islands` = c("Maldives-Lakshadweep-Chagos Archipelago Tropical Moist Forests"),
                        `Philippines & Sulu Sea Tropical Forests` = c("Luzon Rain forests","Luzon Montane Rain forests","Luzon Tropical Pine Forests","Palawan Rain forests","Mindoro Rain forests","Greater Negros-Panay Rain forests","Mindanao-Eastern Visayas Rain forests","Mindanao Montane Rain forests","Sulu Archipelago Rain forests"),
                        `South China Subtropical Evergreen & Monsoon Forests` = c("Hainan Island Monsoon Rain Forests", "South China-Vietnam Subtropical Evergreen Forests","Jian Nan Subtropical Evergreen Forests","Taiwan Subtropical Evergreen Forests","South Taiwan Monsoon Rain Forests"),
                        `Indochina Mixed Forests & Peatlands` = c("Red River Freshwater Swamp Forests","Northern Vietnam Lowland Rain forests","Northern Annamites Rain forests","Southern Annamites Montane Rain forests","Southern Vietnam Lowland Dry Forests","Southeast Indochina dry evergreen forests","Central Indochina Dry Forests","Tonle Sap-Mekong Peat Swamp Forest","Tonle Sap Freshwater Swamp Forests","Cardamom Mountains Rain forests","Chao Phraya Lowland Moist Deciduous Forests","Chao Phraya Freshwater Swamp Forests","Northern Thailand-Laos Moist Deciduous Forests","Luang Prabang Montane Rain forests","Northern Khorat Plateau Moist Deciduous Forests","Indochina Mangroves","South China Sea Islands"),
                        `Irrawaddy & North Indochina Mixed Forests` = c("Irrawaddy Moist Deciduous Forests","Irrawaddy Dry Forests","Northern Indochina Subtropical Forests","Kayah-Karen Montane Rain forests"),
                        `Arakan Mountains & Northern Triangle Forests` = c("Northern Triangle Temperate Forests","Northern Triangle Subtropical Forests","Northeast India-Myanmar Pine Forests","Mizoram-Manipur-Kachin Rain forests","Chin Hills-Arakan Yoma Montane Forests"),
                        `Myanmar Coastal Rainforests & Andaman Sea Islands` = c("Tenasserim-South Thailand Semi-Evergreen Rain forests","Myanmar Coastal Rain forests","Irrawaddy Freshwater Swamp Forests","Andaman Islands Rain forests","Nicobar Islands Rain forests"),
                        `Nansei Islands Subtropical Evergreen Forests` = c("Nansei Islands Subtropical Evergreen Forests"),
                        `Lord Howe & Norfolk Islands` = c("Lord Howe Island Subtropical Forests","Norfolk Island Subtropical Forests"),
                        `New Zealand` = c("Kermadec Islands Subtropical Moist Forests","Northland Temperate Kauri Forests","New Zealand North Island Temperate Forests","Richmond Temperate Forests","Nelson Coast Temperate Forests","Westland Temperate Forests","Fiordland Temperate Forests","New Zealand South Island Montane Grasslands","Canterbury-Otago Tussock Grasslands","New Zealand South Island Temperate Forests","Rakiura Island Temperate Forests","Chatham Island Temperate Forests"),
                        `Southeast Indonesian Dry Forest Islands` = c("Sumba Deciduous Forests","Lesser Sundas Deciduous Forests","Timor and Wetar Deciduous Forests"),
                        `Sulawesi & Maluku Islands` = c("Sulawesi Montane Rain forests","Sulawesi Lowland Rain forests","Buru Rain forests","Halmahera Rain forests","Seram Rain forests","Banda Sea Islands Moist Deciduous Forests"),
                        `New Guinea & Surrounding Islands` = c("Vogelkop-Aru Lowland Rain forests","Vogelkop Montane Rain forests","Southern New Guinea Freshwater Swamp Forests","Biak-Numfoor Rain forests","Yapen Rain forests","Northern New Guinea Lowland Rain and Freshwater Swamp Forests","Northern New Guinea Montane Rain forests","Papuan Central Range Sub-Alpine Grasslands","Central Range Papuan Montane Rain forests","Southern New Guinea Lowland Rain Forests","Trans Fly Savanna and Grasslands","Huon Peninsula Montane Rain forests","Southeast Papuan rain forests","Admiralty Islands Lowland Rain forests","New Britain-New Ireland Lowland Rain forests","New Britain-New Ireland Montane Rain forests","Trobriand Islands Rain forests","Louisiade Archipelago Rain forests","New Guinea Mangroves"),
                        `Vanuatu Islands` = c("Vanuatu Rain forests"),
                        `Coral Sea & New Caledonia Islands` = c("New Caledonia Dry Forests","New Caledonia Rain Forests"),
                        `Solomon Islands` = c("Solomon Islands Rain Forests"),
                        `Queensland Tropical Rainforests & Savannas` = c("Cape York Peninsula Tropical Savanna","Queensland Tropical Rain forests","Brigalow Tropical Savanna"),
                        `North Australian Tropical Savannas` = c("Kimberly Tropical Savanna","Victoria Plains Tropical Savanna","Arnhem Land Tropical Savanna","Carpentaria Tropical Savanna","Mitchell Grass Downs","Einasleigh Upland Savanna"),
                        `Greater Australian Interior Desert & Shrublands` = c("Western Australian Mulga Shrublands","Great Sandy-Tanami Desert","Gibson Desert","Great Victoria Desert","Nullarbor Plains Xeric Shrublands","Central Ranges Xeric Scrub","Tirari-Sturt stony desert","Simpson Desert"),
                        `West Australian Dry Coastal Shrublands` = c("Carnarvon Xeric Shrublands","Pilbara Shrublands"),
                        `South Australian Mediterranean Forests, Woodlands & Scrub` = c("Southwest Australia Savanna","Southwest Australia Woodlands","Jarrah-Karri Forest and Shrublands","Esperance Mallee","Coolgardie Woodlands","Hampton Mallee and Woodlands","Eyre and York mallee","Flinders-Lofty Montane Woodlands"),
                        `East Australian Mediterranean Woodlands & Temperate Savannas` = c("Australian Alps Montane Grasslands","Southeast Australia Temperate Savanna","Murray-Darling Woodlands and Mallee","Naracoorte Woodlands"),
                        `East Australian Temperate Forests & Mountain Shrublands` = c("Eastern Australian Temperate Forests","Southeast Australia Temperate Forests","Australian Alps Montane Grasslands","Tasmanian Temperate Rain forests","Tasmanian Central Highland Forests","Tasmanian Temperate Forests")
                        )

### check whether all the ecoregions are in the polygon dataframe


for(i in 1:length(bioregions_list)){
  
  print(names(bioregions_list)[i])
  
  eco_reg <- bioregions_list[[i]]
 
  if(!all(eco_reg %in% ecoregions$ECO_NAME)){
    print("some ecoregions not in the data frame")
    
    not_found <- which(!(eco_reg %in% ecoregions$ECO_NAME))
    
    for(j in not_found){
      replace <- grep(x = ecoregions$ECO_NAME, pattern = eco_reg[j],ignore.case = TRUE,value = TRUE)
      
      if(length(replace) > 1){
      replace <- grep(x = eco_reg[j], pattern = paste(replace,collapse = "|"),ignore.case = TRUE, value = TRUE)
      }
      
      if(!is_empty(replace)){
       
        
        
       
         eco_reg[j] <- replace
      } else {
        missed_region <- eco_reg[j]
        print(glue::glue("couldn't replace {missed_region}"))
        stop()
      }
      
      }
  
  }
  
  bioregions_list[[i]] <- eco_reg
  
   
}




## we want to be able to visualise these bioregions

eco_list <- bioregions_list[2]



ecoregion_plot <- function(eco_list) {
  
  ecoreg <- unlist(eco_list) 
  
  print(names(eco_list))
  
  polygons <- ecoregions %>% dplyr::filter(ECO_NAME %in% ecoreg) %>% st_combine()


  
    
  poly <- st_as_sf(terra::vect(polygons))
  
  
  wm <-
    map_data("world") %>% filter(region != "Antartica") %>% fortify()
  
  
  
  eco_plot <- ggplot() + coord_fixed() +
    geom_map(
      data = wm,
      map = wm,
      aes(group = group, map_id = region),
      fill = "darkgrey"
    ) +
    geom_sf(data = poly, fill = "red", colour = "red", alpha = 0.8)+ 
    scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 30)) +
    scale_y_continuous(limits = c(-90, 90), breaks = seq(-90, 90, 30)) +
    theme_classic() + ggtitle(label = names(eco_list))
  
  
  ggsave(filename = paste("outputs/bioregion_plots/",names(eco_list), ".png",sep = ""),eco_plot,device = "png", height = 10, width = 40,dpi=300)
  
}

#### save the polygons to see what bioregions we want to some in on.

for(i in 1:length(bioregions_list)){
  ecoregion_plot(eco_list = bioregions_list[i])
}


write_rds(file = "outputs/bioregions_list.rds", bioregions_list)


