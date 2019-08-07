#' CSDR Detailed Definitions
#'
#' The CSDR "Detailed Definitions" csv file which is exported when doing a
#' cross platform query, in CADE, converted into a tibble for easier use in R.
#'
#' @format A tibble of factors with 93 rows and 8 columns:
#' \describe{
#'   \item{`Short Name`}
#'   \describe{
#'      \item{test1}{test 1}
#'      \item{test2}{test 2}}
#'   \item{`Full Name`}{}
#'   \item{`Unit of Measure`}{}
#'   \item{`To Date / At Completion`}{}
#'   \item{`Recurring / Nonrecurring`}{}
#'   \item{`Functional Category`}{}
#'   \item{`Functional Element`}{
#'   \describe{
#'   \item{`Direct Engineering Labor (Hours and Dollars)`}{`This is a Tier 1
#'   (required) Standard Functional Category applicable to Acquisition efforts
#'   only. Engineering includes the scientific exploration, study, analysis,
#'   design, development, evaluation, and redesign of a specific task or WBS
#'   element. Engineering also includes preparation of specifications, drawings,
#'   parts lists, and wiring diagrams; technical coordination between
#'   engineering and manufacturing; design of tools; coordination of suppliers;
#'   planning for and scheduling of tests; analysis of test results; reduction
#'   of data; and preparation of reports. It also includes the determination and
#'   specification of requirements for reliability, maintainability, and quality
#'   control, as well as logistics and training engineering efforts such as
#'   training system and manual development, support equipment design and
#'   development, logistics support analysis and other support engineering
#'   efforts.`}
#'   \item{`Engineering Labor Overhead (Dollars)`}{`This is a Tier 1 (required)
#'   Standard Functional Category applicable to both Acquisition and Sustainment
#'   efforts. It includes all indirect engineering costs and fringe, but
#'   excludes G&A expenses. For example, Engineering Overhead includes the cost
#'   of directing and supporting all Engineering organization-related activities
#'   that cannot be assigned to specific contracts. Engineering overhead
#'   typically includes supervision, policy and procedures, training,
#'   administration, and similar costs.`}
#'   \item{`Direct Manufacturing Other Labor (Hours and Dollars)`}{`This is a
#'   Tier 1 (required) Standard Functional Category applicable to Acquisition
#'   efforts only. The effort expended to manufacture a product or end item that
#'   meets any of the Tier 2 definitions below.`
#'       \describe{
#'       \item{`Direct Manufacturing Support Labor (Hours and Dollars)`}{`This
#'       is a Tier 2 (optional) Standard Functional Category applicable to
#'       Acquisition efforts only. It includes support tothe manufacturing and
#'       maintenance processes such as planning, material handling,
#'       qualitycontrol, and tool maintenance. Quality control entails checking,
#'       physically inspecting,measuring, testing, or otherwise verifying that
#'       products and services conform toestablished technical requirements and
#'       that satisfactory performance is achieved`}
#'       \item{`Direct Manufacturing Tooling Labor (Hours and Dollars)`}{ `This is a
#'       Tier 2 (optional) Standard Functional Category applicable to Acquisition
#'       efforts only. MManufacturing Tooling is a labor cost element of the
#'       Manufacturing Operations functional category that includes the effort and
#'       costs expended to acquire, manufacture, maintain, or replace original
#'       equipment and manufacturing aids. It does not include the cost of procuring
#'       tooling materials or equipment from an external supplier.`}
#'       \item{`Direct Manufacturing Other Labor (Hours and Dollars)`}{`This
#'       is a Tier 2 (optional) Standard Functional Category applicable to
#'       Acquisition efforts only. Entries into this category may pertain to
#'       Direct Manufacturing Touch, Support, or Tooling Labor if the reporting
#'       entity cannot clearly segregate those elements. It may also contain
#'       Direct Manufacturing labor elements that do not match the Touch,
#'       Support, or Tooling elements above.`}
#'                }
#'       }
#'   \item{`Direct Manufacturing Touch Labor (Hours and Dollars)`}{`This is a
#'   Tier 1 (required) Standard Functional Category applicable to Acquisition
#'   efforts only. It includes the fabrication, assembly, integration,
#'   application of paint and coatings, and functional testing of a product or
#'   end item, andmay include certain types of quality control.`}
#'   \item{`Manufacturing Operations Labor Overhead (Dollars)`}{`This is a Tier
#'   1 (required) Standard Functional Category applicable to Acquisition efforts
#'   only. This category covers Direct Manufacturing Touch and Support Labor
#'   functions. Manufacturing Operations Overhead consists of all indirect
#'   costs, including fringe, but excluding G&A expenses. For example,
#'   Manufacturing Operations Overhead includes the cost of directing and
#'   supporting all Manufacturing-organizationrelated activities that cannot be
#'   assigned to specific contracts. Manufacturing Operations Overhead typically
#'   includes supervision, policy and procedures, training, administration, and
#'   similar costs`}
#'   \item{`Direct Maintenance Other Labor (Hours and Dollars)`}{`This is a Tier
#'   1 (required) StandardFunctional Category applicable to Sustainment efforts
#'   only. Direct Maintenance Operations Labor is the effort expended to
#'   maintain a product or end item that meets any of the Tier 2 definitions
#'   below.`
#'       \describe{
#'       \item{`Direct Maintenance Support Labor (Hours and Dollars)`}{`This
#'       is a Tier 2 (optional) Standard Functional Category applicable to
#'       Sustainment efforts only. Maintenance support functions include
#'       planning, material handling, tool maintenance, and quality control
#'       efforts.`}
#'       \item{`Direct Maintenance Other Labor (Hours and Dollars)`}{`This is a
#'       Tier 2 (optional) Standard Functional Category applicable to
#'       Sustainment efforts only. Entries into this category may pertain to
#'       Direct Maintenance Touch or Support Labor if the reporting entity
#'       cannot clearly segregate those elements. It may also contain Direct
#'       Maintenance labor elements that do not match the Touch or Support
#'       elements above.`}
#'                 }
#'         }
#'   \item{`Direct Maintenance Touch Labor (Hours and Dollars)`}{`This is a Tier
#'   1 (Required) Standard Functional Category applicable to Sustainment efforts
#'   only. It includes the effort and costs expended for hands-on
#'   post-production maintenance of final prime mission product items, and may
#'   include certain types of quality control.`}
#'   \item{`Maintenance Operations Labor Overhead (Dollars)`}{`This is a Tier 1
#'   (required) Standard Functional Category applicable to Sustainment efforts
#'   only. It covers Direct Maintenance Touch and Maintenance Support Labor
#'   functions. Maintenance Operations Overhead consists of all indirect costs,
#'   including fringe, but excluding G&A expenses.`}
#'   \item{`Other Direct Costs Not Shown Elsewhere (Hours and Dollars)`}{`This
#'   is a Tier 1 (required) Standard Functional Category applicable to both
#'   Acquisition and Sustainment efforts. Direct costs not assigned to the
#'   Engineering, Manufacturing Operations, Maintenance, or Materials functions
#'   are included in Other Direct Costs Not Shown Elsewhere. This category
#'   includes labor and non-labor costs that meet any of the Tier 2 definitions
#'   below.`
#'       \describe{
#'       \item{`Direct Program Management Labor (Hours and Dollars)`}{`This is a
#'       Tier 2 (optional) Standard Functional Category applicable to both
#'       Acquisition and Sustainment efforts. It includes the business and
#'       administrative planning, organizing, directing, coordinating,
#'       controlling, and approval actions designated to accomplish overall
#'       program objectives, which are not associated with specific hardware
#'       elements and are not included in systems engineering.`}
#'       \item{`Direct Other Labor (Hours and Dollars)`}{`This is a Tier 2
#'       (optional) Standard Functional Category applicable to both Acquisition
#'       and Sustainment efforts. It includes any labor category that does not
#'       align with the engineering, manufacturing operations, maintenance, or
#'       program management functional cost categories. It may include Direct
#'       Program Management labor if the reporting entity cannot clearly
#'       segregate those elements, as well as any other direct labor costs
#'       incurred by the reporting entity that does not match the definitions
#'       listed above.`}
#'       \item{`Direct Services (Dollars)`}{`This is a Tier 2 (optional)
#'       Standard Functional Categoryapplicable to both Acquisition and
#'       Sustainment efforts. It includes subcontracted services not delivering
#'       a physical product that are a direct charge to the program. Examples
#'       might include information technology support, engineering support,
#'       consultants, etc. Do not include services that are an overhead cost.`}
#'       \item{`Other Direct Non-Labor (Dollars)`}{`This is a Tier 2 (optional)
#'       Standard Functional Category applicable to both Acquisition and
#'       Sustainment efforts. It includes direct non-labor costs such as travel,
#'       per diem, reproduction of printed material, rental of special
#'       facilities and equipment, and shipping and transportation charges for
#'       items sent or returned to subcontractors. It may include Direct
#'       Services if the reporting entity cannot clearly segregate those
#'       elements, as well as any other direct non-labor costs incurred by the
#'       reporting entity that does not match the definitions listed above or
#'       below.`}
#'                 }
#'         }
#'   \item{`Other Overhead (Dollars)`}{`This is a Tier 1 (required) Standard
#'   Functional Category applicable to both Acquisition and Sustainment efforts.
#'   Other Overhead includes all functions tagged to Other Direct Costs Not
#'   Shown Elsewhere, including Program Management, Services, Travel, etc. Other
#'   Overhead consists of all indirect costs, including fringe, but excluding
#'   G&A expenses.`}
#'   \item{`Direct Materials (Dollars)`}{`This is a Tier 1 (required) Standard
#'   Functional Category applicable to both Acquisition and Sustainment efforts.
#'   Direct Materials is defined as the dollars paid for material items that
#'   meet any of the Tier 2 definitions below.`
#'       \describe{
#'       \item{`Direct-Reporting Subcontractor (Dollars)`}{`This is a Tier 2
#'       (optional) Standard Functional Category applicable to both Acquisition
#'       and Sustainment efforts. It includes the total price of all
#'       direct-reporting subcontracts. Subcontractor costs that do not directly
#'       report a FlexFile should be reported in one of the other material
#'       categories below based upon the nature and characteristics of the
#'       product provided.`}
#'       \item{`Intercompany Work Orders (IWO) (Dollars)`}{`This is a Tier 2
#'       (optional) Standard Functional Category applicable to both Acquisition
#'       and Sustainment efforts. An IWO is work completed by a separate
#'       business unit of the reporting entity.`}
#'       \item{`Purchased Parts (Dollars)`}{`This is a Tier 2 (optional)
#'       Standard Functional Category applicable to both Acquisition and
#'       Sustainment efforts. Purchased Parts are cost elements of the Materials
#'       functional category that includes items that are discrete components
#'       used in an upper-level assembly. Purchased Parts are distinguished from
#'       purchased equipment by their relatively lower cost and complexity.
#'       Examples include fasteners, clips, clamps, nuts, bolts, washers, nails,
#'       screws, valves, and plumbing and electrical fittings and fixtures.`}
#'       \item{`Purchased Equipment (Dollars)`}{` This is a Tier 2 (optional)
#'       Standard Functional Category applicable to both Acquisition and
#'       Sustainment efforts. Purchased Equipment is a cost element of the
#'       Materials functional category that includes assembled items designed to
#'       be incorporated with other components into a finished product.
#'       Purchased Equipment is distinguished from Purchased Parts by its
#'       relatively higher cost and complexity. Aviation examples include
#'       structural components such as wings, horizontal and vertical tails, and
#'       fuselage; avionics equipment such as radios, inertial navigation
#'       systems, radar systems, and electronic countermeasures; and hydraulic,
#'       pneumatic, and electrical subassemblies such as landing gear, canopy
#'       actuation systems, and wire harnesses. Other examples include brake
#'       disks, tires, transmission, propellers, lenses, scopes, and dishes.`}
#'       \item{`Raw Materials (Dollars)`}{`This is a Tier 2 (optional) Standard
#'       Functional Category applicable to both Acquisition and Sustainment
#'       efforts. Raw Materials are cost elements of the Materials functional
#'       category that includes items that are crude, semi-fabricated, or
#'       partially processed materials or components that have not yet been made
#'       into a definite functional item or configuration. Examples include
#'       consumable items for fabrication, castings, forgings, pressings, sheet
#'       metal, plate, tubing, bars, rebar, rods, wires, cables, fabrics, and
#'       conduits.`}
#'       \item{`Direct Tooling and Equipment (Dollars)`}{`This is a Tier 2
#'       (optional) Standard Functional Category applicable to both Acquisition
#'       and Sustainment efforts. It includes the cost associated with labor,
#'       materials, and equipment used in the manufacture of dies, jigs,
#'       fixtures, molds, gauges, handling equipment, work platforms, and test
#'       equipment for the fabrication and testing of the specific WBS reporting
#'       element.`}
#'       \item{`Other Material (Dollars):`}{`This is a Tier 2 (optional)
#'       Standard Functional Category applicable to both Acquisition and
#'       Sustainment efforts. Other Materials is a cost element of the Materials
#'       functional category that may pertain to Direct-Reporting Subcontractor,
#'       IWO, Purchased Parts, Purchased Equipment, Raw Materials, or Tooling
#'       and Equipment if the reporting entity cannot clearly segregate those
#'       elements. It may also contain other Direct Material elements that do
#'       not match any of the Direct Materials definitions for Tier 2 above.`}
#'                 }
#'         }
#'   \item{`Material Overhead (Dollars)`}{` This is a Tier 1 (required) Standard
#'   Functional Category applicable to both Acquisition and Sustainment efforts.
#'   All indirect material costs relating to Direct Materials (including
#'   Direct-Reporting Subcontractors and IWO), but excluding G&A expenses. For
#'   example, the portion of indirect costs attributable to procured or
#'   subcontracted products, including the cost of purchasing, expediting, and
#'   storing materials, parts, equipment, and assemblies.`}
#'   \item{`General and Administrative (G&A) (Dollars)`}{This is a Tier 1
#'   (required) Standard Functional Category applicable to both Acquisition and
#'   Sustainment efforts. G&A costs are indirect expenses related to the overall
#'   management and administration of the contractor’s business unit, including
#'   the following: a company’s general and executive offices; the cost of staff
#'   services such as legal, accounting, public relations, financial, and
#'   similar expenses; and other general expenses. G&A is also a generic term
#'   used to describe expenses with a beneficial or causal relationship to cost
#'   objectives that cannot be more accurately assigned to overhead areas for
#'   Engineering, Manufacturing Operations, Material, and similar costs.
#'
#'   \strong{\emph{The reporting entity has the choice of whether to report G&A
#'   costs along with the Actuals To Date information, or to withhold G&A
#'   entirely from this Data Group; if the reporting entity elects to report
#'   G&A, then it must be applied for all direct-charges and identification of
#'   G&A as a functional category is required.}}}
#'   \item{`Facilities Capital Cost of Money (FCCOM) (Dollars)`}{This is a Tier 1
#'   (required) Standard Functional Category applicable to both Acquisition and
#'   Sustainment efforts. FCCOM is an imputed cost determined by applying a ‘cost
#'   of money’ rate to facilities capital employed in contract performance
#'   according to Cost Accounting Standard 414, “Cost of Money as an Element of
#'   the Cost of Facilities Capital.” Capital employed is determined without
#'   regard to whether its source is equity or borrowed capital. The resulting
#'   cost of money is not a form of interest on borrowing.
#'
#'   \strong{\emph{The reporting entity has the choice of whether to report FCCOM costs along
#'   with the Actuals To Date information, or to withhold FCCOM entirely from this
#'   Data Group; if the reporting entity elects to report FCCOM, then it must be
#'   applied for all direct-charges and identification of FCCOM as a functional
#'   category is required.}}}}}
#'   \item{`Form`}{The CSDR DD Form type the data field is used in.}
#' }
"detailed_defs"

# Specify CSDR Detailed Definitions types -----------------------------------

col_types <- readr::cols(
  `Short Name` =
    readr::col_factor(
      levels = c(
        "Completion Sequence",
        "CONCUR",
        "F_UNIT",
        "L_UNIT",
        "NR $ AC",
        "NR $ TD",
        "NR Direct Eng $ AC",
        "NR Direct Eng $ TD",
        "NR Eng Hrs AC",
        "NR Eng Hrs TD",
        "NR MFG Direct $ AC",
        "NR MFG Direct $ TD",
        "NR MFG Hrs AC",
        "NR MFG Hrs TD",
        "NR QC Direct $ AC",
        "NR QC Direct $ TD",
        "NR QC Hrs AC",
        "NR QC Hrs TD",
        "NR Tool/Equip $ AC",
        "NR Tool/Equip $ TD",
        "NR Tool Direct $ AC",
        "NR Tool Direct $ TD",
        "NR Tool Hrs AC",
        "NR Tool Hrs TD",
        "NR Direct Rep Sub $ AC",
        "NR Direct Rep Sub $ TD",
        "NR Eng OH $ AC",
        "NR Eng OH $ TD",
        "NR MFG Ops OH $ AC",
        "NR MFG Ops OH $ TD",
        "NR Mat OH $ AC",
        "NR Mat OH $ TD",
        "NR Other $ AC",
        "NR Other $ TD",
        "NR Purch Equip $ AC",
        "NR Purch Equip $ TD",
        "NR Purch Parts $ AC",
        "NR Purch Parts $ TD",
        "NR Raw Mat $ AC",
        "NR Raw Mat $ TD",
        "Units AC",
        "Units TD",
        "Rec Direct Rep Sub $ TD",
        "Rec MFG Direct $ TD",
        "Rec MFG Hrs TD",
        "Rec Other $ TD",
        "Rec Purch Equip $ TD",
        "Rec Purch Parts $ TD",
        "Rec Raw Mat $ TD",
        "Rec $ AC",
        "Rec $ TD",
        "Rec Direct Eng $ TD",
        "Rec Direct Eng $ AC",
        "Rec Direct Eng $ TD",
        "Rec Eng Hrs AC",
        "Rec Eng Hrs TD",
        "Rec MFG Direct $ AC",
        "Rec MFG Direct $ TD",
        "Rec MFG Hrs AC",
        "Rec MFG Hrs TD",
        "Rec QC Direct $ AC",
        "Rec QC Direct $ TD",
        "Rec QC Hrs AC",
        "Rec QC Hrs TD",
        "Rec Tool/Equip $ AC",
        "Rec Tool/Equip $ TD",
        "Rec Tool Direct $ AC",
        "Rec Tool Direct $ TD",
        "Rec Tool Hrs AC",
        "Rec Tool Hrs TD",
        "Rec Direct Rep Sub $ AC",
        "Rec Direct Rep Sub $ TD",
        "Rec Eng Hrs TD",
        "Rec Eng OH $ AC",
        "Rec Eng OH $ TD",
        "Rec MFG Ops OH $ AC",
        "Rec MFG Ops OH $ TD",
        "Rec Mat OH $ AC",
        "Rec Mat OH $ TD",
        "Rec Other $ AC",
        "Rec Other $ TD",
        "Rec Purch Equip $ AC",
        "Rec Purch Equip $ TD",
        "Rec Purch Parts $ AC",
        "Rec Purch Parts $ TD",
        "Rec QC Direct $ TD",
        "Rec QC Hrs TD",
        "Rec Raw Mat $ AC",
        "Rec Raw Mat $ TD",
        "Rec Tool Direct $ TD",
        "Rec Tool Hrs TD",
        "Rec Tool/Equip $ TD",
        "Remarks"
      )
    ),

  `Full Name` =
    readr::col_factor(
      levels = c(
        "Competed Units/Lots in sequence A1,.,An.",
        "Concurrent Units/Lots",
        "First Unit",
        "Last Unit",
        "Nonrecurring Costs Incurred At Completion",
        "Nonrecurring Costs Incurred To Date",
        "Nonrecurring Direct Engineering Labor Dollars Incurred At Completion",
        "Nonrecurring Direct Engineering Labor Dollars Incurred To Date",
        "Nonrecurring Direct Engineering Labor Hours Incurred At Completion",
        "Nonrecurring Direct Engineering Labor Hours Incurred To Date",
        "Nonrecurring Direct Manufacturing Labor Dollars Incurred At Completion",
        "Nonrecurring Direct Manufacturing Labor Dollars Incurred To Date",
        "Nonrecurring Direct Manufacturing Labor Hours Incurred At Completion",
        "Nonrecurring Direct Manufacturing Labor Hours Incurred To Date",
        "Nonrecurring Direct Quality Control Labor Dollars Incurred At Completion",
        "Nonrecurring Direct Quality Control Labor Dollars Incurred To Date",
        "Nonrecurring Direct Quality Control Labor Hours Incurred At Completion",
        "Nonrecurring Direct Quality Control Labor Hours Incurred To Date",
        "Nonrecurring Direct Tooling & Equipment Dollars Incurred At Completion",
        "Nonrecurring Direct Tooling & Equipment Dollars Incurred To Date",
        "Nonrecurring Direct Tooling Labor Dollars Incurred At Completion",
        "Nonrecurring Direct Tooling Labor Dollars Incurred To Date",
        "Nonrecurring Direct Tooling Labor Hours Incurred At Completion",
        "Nonrecurring Direct Tooling Labor Hours Incurred To Date",
        "Nonrecurring Direct-Reporting Subcontractor Dollars Incurred At Completion",
        "Nonrecurring Direct-Reporting Subcontractor Dollars Incurred To Date",
        "Nonrecurring Engineering Overhead Dollars Incurred At Completion",
        "Nonrecurring Engineering Overhead Dollars Incurred To Date",
        "Nonrecurring Manufacturing Operations Overhead Dollars Incurred At Completion",
        "Nonrecurring Manufacturing Operations Overhead Dollars Incurred To Date",
        "Nonrecurring Material Handling/Overhead Dollars Incurred At Completion",
        "Nonrecurring Material Handling/Overhead Dollars Incurred To Date",
        "Nonrecurring Other Costs Not Shown Elsewhere Incurred At Completion",
        "Nonrecurring Other Costs Not Shown Elsewhere Incurred To Date",
        "Nonrecurring Purchased Equipment Dollars Incurred At Completion",
        "Nonrecurring Purchased Equipment Dollars Incurred To Date",
        "Nonrecurring Purchased Parts Dollars Incurred At Completion",
        "Nonrecurring Purchased Parts Dollars Incurred To Date",
        "Nonrecurring Raw Material Dollars Incurred At Completion",
        "Nonrecurring Raw Material Dollars Incurred To Date",
        "Number of Units At Completion",
        "Number of Units To Date",
        "Recuring Direct Reporting Subcontractor Dollars To Date",
        "Recuring Manufacturing Direct Dollars To Date",
        "Recuring Manufacturing Hours To Date",
        "Recuring Other Dollars To Date",
        "Recuring Purchased Equipment Dollars To Date",
        "Recuring Purchased Parts Dollars To Date",
        "Recuring Raw Material Dollars To Date",
        "Recurring Costs Incurred At Completion",
        "Recurring Costs Incurred To Date",
        "Recurring Direct Engineering Dollars To Date",
        "Recurring Direct Engineering Labor Dollars Incurred At Completion",
        "Recurring Direct Engineering Labor Dollars Incurred To Date",
        "Recurring Direct Engineering Labor Hours Incurred At Completion",
        "Recurring Direct Engineering Labor Hours Incurred To Date",
        "Recurring Direct Manufacturing Labor Dollars Incurred At Completion",
        "Recurring Direct Manufacturing Labor Dollars Incurred To Date",
        "Recurring Direct Manufacturing Labor Hours Incurred At Completion",
        "Recurring Direct Manufacturing Labor Hours Incurred To Date",
        "Recurring Direct Quality Control Labor Dollars Incurred At Completion",
        "Recurring Direct Quality Control Labor Dollars Incurred To Date",
        "Recurring Direct Quality Control Labor Hours Incurred At Completion",
        "Recurring Direct Quality Control Labor Hours Incurred To Date",
        "Recurring Direct Tooling & Equipment Dollars Incurred At Completion",
        "Recurring Direct Tooling & Equipment Dollars Incurred To Date",
        "Recurring Direct Tooling Labor Dollars Incurred At Completion",
        "Recurring Direct Tooling Labor Dollars Incurred To Date",
        "Recurring Direct Tooling Labor Hours Incurred At Completion",
        "Recurring Direct Tooling Labor Hours Incurred To Date",
        "Recurring Direct-Reporting Subcontractor Dollars Incurred At Completion",
        "Recurring Direct-Reporting Subcontractor Dollars Incurred To Date",
        "Recurring Engineering Hours To Date",
        "Recurring Engineering Overhead Dollars Incurred At Completion",
        "Recurring Engineering Overhead Dollars Incurred To Date",
        "Recurring Manufacturing Operations Overhead Dollars Incurred At Completion",
        "Recurring Manufacturing Operations Overhead Dollars Incurred To Date",
        "Recurring Material Handling/Overhead Dollars Incurred At Completion",
        "Recurring Material Handling/Overhead Dollars Incurred To Date",
        "Recurring Other Costs Not Shown Elsewhere Incurred At Completion",
        "Recurring Other Costs Not Shown Elsewhere Incurred To Date",
        "Recurring Purchased Equipment Dollars Incurred At Completion",
        "Recurring Purchased Equipment Dollars Incurred To Date",
        "Recurring Purchased Parts Dollars Incurred At Completion",
        "Recurring Purchased Parts Dollars Incurred To Date",
        "Recurring Quality Control Direct Dollars To Date",
        "Recurring Quality Control Hours To Date",
        "Recurring Raw Material Dollars Incurred At Completion",
        "Recurring Raw Material Dollars Incurred To Date",
        "Recurring Tooling Direct Dollars To Date",
        "Recurring Tooling Hours To Date",
        "Recurring Tooling&Equipment Dollars To Date",
        "Remarks"
      )
    ),
  `Unit of Measure` =
    readr::col_factor(levels = c("TY $K",
                                 "Hours",
                                 "Quantity")),

  `To Date / At Completion` =
    readr::col_factor(levels = c("At Completion",
                                 "To Date")),

  `Recurring / Nonrecurring` =
    readr::col_factor(levels = c("Nonrecurring",
                                 "Recurring")),

  `Functional Category` =
    readr::col_factor(
      levels = c(
        "Summary",
        "Engineering",
        "Manufacturing",
        "Quality Control",
        "Tooling",
        "Material",
        "Manufacturing Operations",
        "Other"
      )
    ),

  `Functional Element` =
    readr::col_factor(
      levels = c(
        "Summary",
        "Direct Engineering Labor",
        "Direct Manufacturing Labor",
        "Direct Quality Control Labor",
        "Direct Tooling & Equipment",
        "Direct Tooling Labor",
        "Direct-Reporting Subcontractor",
        "Engineering Overhead",
        "Manufacturing Operations Overhead",
        "Material Handling/Overhead",
        "Other Costs Not Shown Elsewhere",
        "Purchased Equipment",
        "Purchased Parts",
        "Raw Material"
      )
    ),

  `Form` =
    readr::col_factor(levels = c("1921-2", "1921,1921-1"))
)

detailed_defs <-
  readr::read_csv("extdata/Detailed_Definitions.csv",
                  col_types = col_types,
                  na = "N/A")

rm(col_types)

use_data(detailed_defs, overwrite = TRUE)
