package drt.shared

import drt.shared.Terminals.{T2, T5, Terminal}

object RedList {
  def redListOriginWorkloadExcluded(portCode: PortCode, terminal: Terminal): Boolean =
    portCode == PortCode("LHR") && List(T2, T5).contains(terminal)

  val countryToCode: Map[String, String] = Map(
    "Afghanistan" -> "AFG",
    "Angola" -> "AGO",
    "Argentina" -> "ARG",
    "Bahrain" -> "BHR",
    "Bangladesh" -> "BGD",
    "Bolivia" -> "BOL",
    "Botswana" -> "BWA",
    "Brazil" -> "BRA",
    "Burundi" -> "BDI",
    "Cape Verde" -> "CPV",
    "Chile" -> "CHL",
    "Colombia" -> "COL",
    "Costa Rica" -> "CRI",
    "Democratic Republic of the Congo" -> "COD",
    "Ecuador" -> "ECU",
    "Egypt" -> "EGY",
    "Eswatini" -> "SWZ",
    "Ethiopia" -> "ETH",
    "French Guiana" -> "GUF",
    "Guyana" -> "GUY",
    "India" -> "IND",
    "Kenya" -> "KEN",
    "Lesotho" -> "LSO",
    "Malawi" -> "MWI",
    "Mozambique" -> "MOZ",
    "Namibia" -> "NAM",
    "Oman" -> "OMN",
    "Pakistan" -> "PAK",
    "Panama" -> "PAN",
    "Paraguay" -> "PRY",
    "Peru" -> "PER",
    "Philippines" -> "PHL",
    "Qatar" -> "QAT",
    "Rwanda" -> "RWA",
    "Seychelles" -> "SYC",
    "Somalia" -> "SOM",
    "South Africa" -> "ZAF",
    "Sri Lanka" -> "LKA",
    "Sudan" -> "SDN",
    "Suriname" -> "SUR",
    "Tanzania" -> "TZA",
    "Turkey" -> "TUR",
    "Trinidad and Tobago" -> "TTO",
    "United Arab Emirates" -> "ARE",
    "Uruguay" -> "URY",
    "Venezuela" -> "VEN",
    "Zambia" -> "ZMB",
    "Zimbabwe" -> "ZWE",
  )
}
