# how to fix the ecological site IDs

# bring in the climate and indicators data 
# also need to bring in this synonym list to convert old IDs to new ones
# Specify the file path
file_path <- "class-synonym-list.txt"

# Read the text file into a DataFrame
legacy_ecosite_lut <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE)







# Get only MLRA 42
Part1Data_indic_mlra42 <- subset(Indic_RinRang,
                                 (mlrarsym %in% c("42")))


# OKAY! So let's get these corrected because some of the ecological site IDs
# claim to be from places like MLRA 042X which EDIT doesn't recognize.
#First, let's grab all the MLRAs in EDIT
all_mlras <- trex::fetch_mlra_codes()
mlras_042 <- all_mlras[grepl(x = all_mlras,
                             pattern = "042")]
# If that's not working (it claims it's not in the namespace for trex for me) we
# can just hardcode it.
mlras_042 <- c("042A", "042B", "042C")


# We can find all the ecosite IDs which have the MLRA 042 but DON'T match any
# MLRA codes we already know are in EDIT
# We start by asking which letters are represented in EDIT in this MLRA
expected_terminal_letters <- stringr::str_extract(string = mlras_042,
                                                  pattern = "[A-Z]$")
# This regular expression uses those to look for instances of 042 followed by an
# UNexpected letter
funky_mlra_regex <- paste0("042[^", paste(expected_terminal_letters, collapse = ""),"]")

# Get a lookup table of just the wonky ecosites which will be the ones where the
# value in Synonym matches our regular expression
# The dplyr::select() is just an easy way to get only the variables we want and
# to rename them for convenience
funky_mlra_042_ecosite_lut <- unique(dplyr::select(.data = dplyr::filter(.data = legacy_ecosite_lut,
                                                                         grepl(x = Synonym,
                                                                               pattern = funky_mlra_regex)),
                                                   EcologicalSiteId = Synonym,
                                                   corrected_ecosite_id = Ecological.site.ID))

# Now we can join the table to our data to add the corrected_ecosite_id variable
Part1Data_indic_mlra42 <- dplyr::left_join(x = Part1Data_indic_mlra42,
                                           y = funky_mlra_042_ecosite_lut)

# And for all the places where the ecosite IDs were (probably) fine we should
# now have an NA in the correct_ecosite_id variable, so we can just write in
# the originally-assigned ecosite ID there
Part1Data_indic_mlra42$corrected_ecosite_id[is.na(Part1Data_indic_mlra42$corrected_ecosite_id)] <- Part1Data_indic_mlra42$EcologicalSiteId[is.na(Part1Data_indic_mlra42$corrected_ecosite_id)]

# And extract the proper MLRA code from the corrected ecosite ID
Part1Data_indic_mlra42$corrected_mlra <- stringr::str_extract(string = Part1Data_indic_mlra42$corrected_ecosite_id,
                                                              pattern = "(?<=^[RF])\\d{3}[A-Z]")

climate_data = read_csv("RinRange_Pt1Data_climate_mlra42.csv")
climate_data = climate_data %>% pivot_wider(names_from = "Property", values_from = "Average")


colnames(climate_data)
# Perform the inner join
merged_indic_climate_data <- Part1Data_indic_mlra42 %>%
  inner_join(climate_data, by = c("corrected_ecosite_id" = "Ecological site ID"))


write.csv(merged_indic_climate_data, "merged_indic_climate_data.csv", row.names=F)