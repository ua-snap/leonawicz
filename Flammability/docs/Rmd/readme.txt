#################################################################################################
#### This readme describes the methods used to create a final vegetation classification map. ####
#################################################################################################

#### Author: 		Matthew Leonawicz ####
#### Last updated:  06/17/2015        ####

#### Creating a new vegetation classification map layer ####

#### Source data:
(1) Original CAVM layer
(2) Alfresco layer

These two maps have different classification codes.
CAVM ranges from 0 to 21, not all levels occurring in the map.
CAVM + Alfresco ranges from 0 to 4. A given code in one map does not correspond to the same vegetation class in the other map.
Therefore, integer codes in the CAVM are reclassed such that shrub, graminoid, and wetland correspond to classes 5, 6, and 7, respectively in both maps.
In the CAVM, all remaining vegetation classes are reclassed to NA.

Using the R programming language and the raster package, the two maps are combined using the merge() command.
This is done such that
(1) the CAVM layer has priority over the CAVM + Alfresco layer for non-NA cells,
so that the resulting map takes on the shrub, graminoid, and wetland values (5, 6 and 7) from the CAVM,
(2) but does not retain priority for NA cells,
so that the resulting map then falls back on the Alfresco layer for non-NA data.

This yields a map layer that is essentially the Alfresco layer over most of the spatial domain,
replaced with the values in CAVM layer only in cells defined by the CAVM layer to be shrub, graminoid, or wetland.

The CAVM layer does not account for inconsistency in fuel availability across the landscape.
After these layers are combined, all cells in the CAVM + Alfresco map corresponding to class 0 in the Alfresco layer are reset to zero.
This is because the initial merge of Alfresco and CAVM map layers undesirably replaces some CAVM-prioritized cells which are 0-valued in the Alfresco layer with integer codes 5, 6 or 7.
This final step ensures replacement of water features which more realistically break up the landscape fuel availability in the CAVM region.

Final Classification codes match that of the CAVN+Alfresco layer:
1 = Alpine tundra
2 = Black spruce
3 = White spruce
4 = Deciduous
5 = Shrub tundra
6 = Grammanoid tundra
7 = Wetland tundra
