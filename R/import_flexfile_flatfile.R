#' Import a CSDR FlexFile Flat File
#'
#' Takes a CSDR FlexFile Flat File csv and imports it to a tibble.
#'
#' @title import_flexfile_ff
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param path Required. Path to the csv file which contains the CSDR FlexFile
#'    flat file.
#' @return Returns a tibble of the CSDR FlexFile data.
#' @export

# @import readr

import_flexfile_flatfile <- function(path) {                  ### CSDR Cost and Hour Report (Flex File)
  flexfile_flatfile <-                                        ### File Format Specification
    readr::read_csv(                                          ### Version 1.0 (2019-03-05)
      file = path,                                              # Data Type / Nullable
      col_types = cols(                                         # --------- / --------
        OrderOrLot_ID                   = readr::col_character(),      # StringID  / No
        OrderOrLot_Name                 = readr::col_character(),      # String    / No
        CLIN_ID                         = readr::col_character(),      # StringID  / No
        CLIN_Name                       = readr::col_character(),      # String    / No
        EndItem_ID                      = readr::col_character(),      # StringID  / No
        EndItem_Name                    = readr::col_character(),      # String    / No
        WBSElement_ID                   = readr::col_character(),      # StringID  / No
        WBSElement_Name                 = readr::col_character(),      # String    / No
        WBSElement_Level                = readr::col_integer(),        # Integer   / No
        WBSElement_ID_Level1            = readr::col_character(),      # StringID  / No
        WBSElement_Name_Level1          = readr::col_character(),      # String    / No
        WBSElement_ID_Level2            = readr::col_character(),      # StringID  / No
        WBSElement_Name_Level2          = readr::col_character(),      # String    / No
        WBSElement_ID_Level3            = readr::col_character(),      # StringID  / No
        WBSElement_Name_Level3          = readr::col_character(),      # String    / No
        WBSElement_ID_Level4            = readr::col_character(),      # StringID  / No
        WBSElement_Name_Level4          = readr::col_character(),      # String    / No
        WBSElement_ID_Level5            = readr::col_character(),      # StringID  / No
        WBSElement_Name_Level5          = readr::col_character(),      # String    / No
        WBSElement_ID_Level6            = readr::col_character(),      # StringID  / No
        WBSElement_Name_Level6          = readr::col_character(),      # String    / No
        WBSElement_ID_Level7            = readr::col_character(),      # StringID  / No
        WBSElement_Name_Level7          = readr::col_character(),      # String    / No
        WBSElement_ID_Level8            = readr::col_character(),      # StringID  / No
        WBSElement_Name_Level8          = readr::col_character(),      # String    / No
        Account_ID                      = readr::col_character(),      # StringID  / No
        Account_Name                    = readr::col_character(),      # String    / No
        NonrecurringOrRecurring_ID      = readr::col_factor(c(         # See "2.4.5 NonrecurringOrRecurringEnum"
                                           "NONRECURRING",
                                            "RECURRING")),
        FunctionalCategory_ID           = readr::col_character(),      # StringID  / No
        FunctionalCategory_Name         = readr::col_character(),      # String    / No
        FunctionalOverheadCategory_ID   = readr::col_character(),      # StringID  / No
        FunctionalOverheadCategory_Name = readr::col_character(),      # String    / No
        StandardCategory_ID             = readr::col_factor(c(         # See "2.4.6 NonrecurringOrRecurringEnum"
                               "DIRECT_ENGINEERING_LABOR",
                             "ENGINEERING_LABOR_OVERHEAD",
                       "DIRECT_MANUFACTURING_TOUCH_LABOR",
                       "DIRECT_MANUFACTURING_OTHER_LABOR",
                "MANUFACTURING_OPERATIONS_LABOR_OVERHEAD",
                         "DIRECT_MAINTENANCE_TOUCH_LABOR",
                         "DIRECT_MAINTENANCE_OTHER_LABOR",
                  "MAINTENANCE_OPERATIONS_LABOR_OVERHEAD",
                                     "OTHER_DIRECT_COSTS",
                                         "OTHER_OVERHEAD",
                                       "DIRECT_MATERIALS",
                                      "MATERIAL_OVERHEAD",
                             "GENERAL_AND_ADMINISTRATIVE",
                     "FACILITIES_CAPITAL_COST_OF_MONEY")),
        DetailedStandardCategory_ID     = readr::col_factor(c(         # See "2.4.7 DetailedStandardCategoryEnum"
                               "DIRECT_ENGINEERING_LABOR",
                             "ENGINEERING_LABOR_OVERHEAD",
                       "DIRECT_MANUFACTURING_TOUCH_LABOR",
                     "DIRECT_MANUFACTURING_SUPPORT_LABOR",
                     "DIRECT_MANUFACTURING_TOOLING_LABOR",
                       "DIRECT_MANUFACTURING_OTHER_LABOR",
                "MANUFACTURING_OPERATIONS_LABOR_OVERHEAD",
                         "DIRECT_MAINTENANCE_TOUCH_LABOR",
                       "DIRECT_MAINTENANCE_SUPPORT_LABOR",
                         "DIRECT_MAINTENANCE_OTHER_LABOR",
                  "MAINTENANCE_OPERATIONS_LABOR_OVERHEAD",
                        "DIRECT_PROGRAM_MANAGEMENT_LABOR",
                                     "DIRECT_OTHER_LABOR",
                                        "DIRECT_SERVICES",
                                 "OTHER_DIRECT_NON_LABOR",
                                         "OTHER_OVERHEAD",
                         "DIRECT_REPORTING_SUBCONTRACTOR",
                               "INTERCOMPANY_WORK_ORDERS",
                                        "PURCHASED_PARTS",
                                    "PURCHASED_EQUIPMENT",
                                          "RAW_MATERIALS",
                           "DIRECT_TOOLING_AND_EQUIPMENT",
                                         "OTHER_MATERIAL",
                                      "MATERIAL_OVERHEAD",
                             "GENERAL_AND_ADMINISTRATIVE",
                     "FACILITIES_CAPITAL_COST_OF_MONEY")),
        UnitOrSublot_ID                 = readr::col_character(),      # StringID  / No
        UnitOrSublot_FirstUnitNumber    = readr::col_integer(),        # Integer   / No
        UnitOrSublot_LastUnitNumber     = readr::col_integer(),        # Integer   / No
        AllocationMethod_ID             = readr::col_character(),      # StringID  / No
        AllocationMethod_Name           = readr::col_character(),      # String    / yes
        ReportingPeriod_ID              = readr::col_integer(),        # Integer   / No
        ReportingPeriod_StartDate       = readr::col_date("%Y-%m-%d"), # Date      / No
        ReportingPeriod_EndDate         = readr::col_date("%Y-%m-%d"), # Date      / No
        Tag1                            = readr::col_character(),      # String    / Yes
        Tag2                            = readr::col_character(),      # String    / Yes
        Tag3                            = readr::col_character(),      # String    / Yes
        Tag4                            = readr::col_character(),      # String    / Yes
        Tag5                            = readr::col_character(),      # String    / Yes
        Tag6                            = readr::col_character(),      # String    / Yes
        Tag7                            = readr::col_character(),      # String    / Yes
        Tag8                            = readr::col_character(),      # String    / Yes
        Tag9                            = readr::col_character(),      # String    / Yes
        Tag10                           = readr::col_character(),      # String    / Yes
        Tag11                           = readr::col_character(),      # String    / Yes
        Tag12                           = readr::col_character(),      # String    / Yes
        Tag13                           = readr::col_character(),      # String    / Yes
        Tag14                           = readr::col_character(),      # String    / Yes
        Tag15                           = readr::col_character(),      # String    / Yes
        Tag16                           = readr::col_character(),      # String    / Yes
        Tag17                           = readr::col_character(),      # String    / Yes
        Tag18                           = readr::col_character(),      # String    / Yes
        Tag19                           = readr::col_character(),      # String    / Yes
        Tag20                           = readr::col_character(),      # String    / Yes
        Tag21                           = readr::col_character(),      # String    / Yes
        Tag22                           = readr::col_character(),      # String    / Yes
        Tag23                           = readr::col_character(),      # String    / Yes
        Tag24                           = readr::col_character(),      # String    / Yes
        Tag25                           = readr::col_character(),      # String    / Yes
        ActualToDate_Dollars            = readr::col_double(),         # Decimal   / Yes
        ActualToDate_Hours              = readr::col_double()          # Decimal   / Yes
      )
    )

return(flexfile_flatfile)

}
