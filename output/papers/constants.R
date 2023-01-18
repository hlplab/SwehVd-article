# Constants ------------------------------------------------------------------
theme_set(
  theme_bw(base_size = 11) + 
    theme(
      panel.grid = element_blank()))

levels.condition.wade = c("native mean & correlation, minimal variance", "native mean & correlation, native variance", "native mean & correlation, nonnative variance", "nonnative mean & correlation, minimal variance", "nonnative mean & correlation, native variance", "nonnative mean & correlation, nonnative variance")
levels.vowel.IPA = c("[i]", "[ɪ]", "[ɛ]", "[æ]", "[ʌ]", "[ʊ]", "[u]", "[ɑ]")
levels.vowel.Arpabet = c("iy1", "ih1", "eh1", "ae1", "ah1", "uh1", "uw1", "aa1")
levels.vowel.SweFA = c("ii1", "ih1", "yy1", "yh1", "uu1", "uh1", "ee1", "eh1", "ae1", "{:", "{", "oe1", "oeh1", "9:", "9", "aa1", "ah1", "oa1", "oah1", "oo1", "oh1")
levels.vowel.IPA.swe = c("[iː]", "[ɪ]", "[yː]", "[ʏ]", "[ʉː]", "[ɵ]", "[eː]", "[ɛ]", "[ɛː]", "[æː]", "[æ]", "[øː]", "[ø]", "[œː]", "[œ]", "[ɑː]", "[a]", "[oː]", "[ɔ]", "[uː]", "[ʊ]")
levels.vowel.IPA.swe.long = c("[iː]", "[yː]", "[ʉː]", "[eː]", "[ɛː]", "[æː]", "[øː]", "[œː]", "[ɑː]", "[oː]", "[uː]")
levels.vowel.IPA.swe.long.noallo = c("[iː]", "[yː]", "[ʉː]", "[eː]", "[ɛː]", "[øː]", "[ɑː]", "[oː]", "[uː]")
levels.vowel.IPA.swe.short = c("[ɪ]", "[ʏ]", "[ɵ]", "[ɛ]", "[æ]", "[ø]", "[œ]", "[a]", "[ɔ]", "[ʊ]")
levels.vowel.SweFA.wrongEncod = c("ii1", "ih1", "yy1", "yh1", "uu1", "uh1", "ee1", "eh1", "ae1", "{:", "{", "oe1", "oeh1", "09:00", "9", "aa1", "ah1", "oa1", "oah1", "oo1", "oh1")

levels.Phase <- c("practice", "exposure", "test")
levels.Sex <- c("Female", "Male")
levels.Ethnicity <- c("Hispanic", "Non-Hispanic")
levels.Race <- c("American Indian", "Asian", "Black", "other", "White", "multiple")

# While R can handle unicode, stan cannot. Thus using IPA as values does not work for models
levels.response.vowel <- c("heed", "hid", "head", "had", "hud", "hood", "whod", "hod")
labels.response.vowel <- c("i", "ɪ", "ɛ", "æ", "ʌ", "ʊ", "u", "ɑ")
levels.response <- c("heed", "hid", "head", "had", "hud", "hood", "whod", "hod")
labels.response <- c("heed", "hid", "head", "had", "hud", "hood", "whod", "hod")
levels.response.natural <- c("heed", "hid", "head", "had", "hut", "hood", "who'd", "odd")
labels.response.natural <- c("heed", "hid", "head", "had", "hut", "hood", "who'd", "odd")
levels.response.natural.swe.long <- c("hid", "hyd", "hud", "hed", "häd", "härd", "höd", "hörd", "had", "håd", "hod")
levels.response.natural.swe.long.noallo <- c("hid", "hyd", "hud", "hed", "häd", "höd", "had", "håd", "hod")
levels.cue.names = c("Item.Cue_Hz_F1", "Item.Cue_Hz_F2", "Item.Cue_Hz_F3", "Item.Cue_Mel_F1", "Item.Cue_Mel_F2", "Item.Cue_Mel_F3")
levels.formants = c("F0", "F1", "F2", "F3")

levels.cue.names.transform = c("F1_Hz", "F2_Hz", "F3_Hz", "F1_Mel", "F2_Mel", "F3_Mel", "F1_Bark", "F2_Bark", "F3_Bark", "F1_ERB", "F2_ERB", "F3_ERB", "F1_semitones", "F2_semitones", "F3_semitones")
levels.normalization = c("Hz_r", "Mel_r", "Bark_r", "ERB_r", "semitones_r", "Bark_SyrdalGopal", "Bark_SyrdalGopal2", "log_Miller", "Hz_CCuRE", "Mel_CCuRE", "Bark_CCuRE", "ERB_CCuRE", "semitones_CCuRE", "log_Nearey1", "log_Nearey2", "Hz_Gerstman", "Hz_Lobanov")
levels.normalization.plots = c("r_Hz", "r_Mel", "r_Bark", "r_ERB", "r_semitones", "SyrdalGopal_Bark", "SyrdalGopal2_Bark", "Miller_log", "CCuRE_Hz", "CCuRE_Mel", "CCuRE_Bark", "CCuRE_ERB", "CCuRE_semitones", "Nearey1_log", "Nearey2_log", "Gerstman_Hz", "Lobanov_Hz")
levels.normalization.1SG = c("r_Hz", "r_Mel", "r_Bark", "r_ERB", "r_semitones", "SyrdalGopal_Bark", "Miller_log", "CCuRE_Hz", "CCuRE_Mel", "CCuRE_Bark", "CCuRE_ERB", "CCuRE_semitones", "Nearey1_log", "Nearey2_log", "Gerstman_Hz", "Lobanov_Hz")
labels.normalization = c("no normalization (Hz)", "transformed (Mel)", "transformed (Bark)", "transformed (ERB)","transformed (semitones)", "SyrdalGopal (Bark)", "SyrdalGopal2 (Bark)", "Miller (log)", "C-CuRE (Hz)", "C-CuRE (Mel)", "C-CuRE (Bark)", "C-CuRE (ERB)", "C-CuRE (semitones)", "Nearey1 (log)", "Nearey2 (log)", "Gerstman (Hz)", "Lobanov (Hz)")
labels.normalization.1SG = c("no normalization (Hz)", "transformed (Mel)", "transformed (Bark)", "transformed (ERB)","transformed (semitones)", "SyrdalGopal (Bark)", "Miller (log)", "C-CuRE (Hz)", "C-CuRE (Mel)", "C-CuRE (Bark)", "C-CuRE (ERB)", "C-CuRE (semitones)", "Nearey1 (log)", "Nearey2 (log)", "Gerstman (Hz)", "Lobanov (Hz)")
levels.norm = c("r", "SyrdalGopal", "SyrdalGopal2", "Miller", "CCuRE", "Nearey1", "Nearey2", "Gerstman", "Lobanov")

# Color codes for plotting
colors.vowel <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
names(colors.vowel) <- levels.vowel.IPA
colors.vowel.word <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
names(colors.vowel.word) <- levels.response.natural
# repeat the same code to assign the same colours to IO.Vowel in order to plot posteriors in the same colours
colors.vowel.IO <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
names(colors.vowel.IO) <- levels.response
# Colors for plotting IOs categorization accuracy
colors.all.procedures.1SG <- c("#C9C0BB", "#C9C0BB", "#C9C0BB", "#C9C0BB", "#C9C0BB", "#E6BE8A", "#E6BE8A", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#DDADAF", "#DDADAF")
names(colors.all.procedures.1SG) <- labels.normalization.1SG
colors.all.procedures <- c("#C9C0BB", "#C9C0BB", "#C9C0BB", "#C9C0BB", "#C9C0BB", "#E6BE8A", "#E6BE8A","#E6BE8A", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#ABCDEF", "#DDADAF", "#DDADAF")
names(colors.all.procedures) <- labels.normalization
colors.normalization <- c("unnormalized" = "royalblue1", "centered" = "orangered1", "scaled" = "palegreen3")
colors.transform <- c("Hz" = "royalblue1", "Mel" = "orangered1", "Bark" = "palegreen3", "log" = "lightblue1", "semitones" = "orange1", "ERB" = "green1")
colors.norm <- c("Lobanov" = "yellow1", "Nearey1" = "royalblue4", "Nearey2" = "lightyellow1", "Gerstman" = "maroon4", "C-CuRE" = "tomato3", "Miller" = "cadetblue4")
colors.IO.Type <- c("talker-independent-1" = "#999999", "talker-independent-2" = "#E69F00", "talker-independent-3" = "#56B4E9", "talker-independent-4" = "#009E73", "talker-independent-5" = "#D55E00")
colScale <-   scale_colour_manual(name = "Vowel",values = colors.vowel)
colors.vowel.swe <- c("#66C2A5", "#1B9E77", "#FC8D62", "#D95F02", "#8DA0CB", "#7570B3", "#FB9A99", "#E78AC3", "#E7298A", "#A6D854", "#66A61E", "#FFD92F", "#E6AB02", "#E5C494", "#A6761D", "#B3B3B3", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C")
names(colors.vowel.swe) <- levels.vowel.IPA.swe

# levels.vowel.IPA.reverse <- c("[ɑ]", "[u]", "[ʊ]", "[ʌ]", "[æ]", "[ɛ]", "[ɪ]", "[i]")
# levels.response.vowel.reverse <- c("hod", "whod", "hood", "hud", "had", "head", "hid", "heed")

# Color codes from PhonR
#first define no of colors needed, set to max 11 ---CHANGE THIS INTO A FUNCTION-------
num.col = 8
hue <- seq(0,  360, length.out=1+num.col)[-1]
chr <- seq(60, 100, length.out=num.col)
lum <- seq(60,  40, length.out=num.col)
pretty.col <- hcl(hue, chr, lum, alpha=1)
names(pretty.col) <- levels.vowel.IPA

# Colors for grouping norm procedures
color.transform <- "#C2B280"
color.centering <- "#6699CC"
color.centering.more <- "#E97451"

vowelPlot_components <- list(
  axis = c(scale_x_reverse(), scale_y_reverse()),
  scale = colScale, 
  theme(legend.position="right"), 
  guides(color = guide_legend(order=1), shape = guide_legend(order=2)))

## STAN / BRMS constants ---------------------------------------------------------------
chains <- 4
options(
  width = 110,
  mc.cores = min(chains, parallel::detectCores()))

require(brms)
my_priors <- c(
  prior(student_t(3, 0, 2.5), class = "b"),
  prior(cauchy(0, 2.5), class = "sd"),
  prior(lkj(1), class = "cor")
)

# Functions ------------------------------------------------------------------
myGplot.defaults = function(
    type = c("paper","poster","slides")[1],
    base_size = if (type == "paper") { 10 } else if (type == "slides") { 32 } else if (type == "poster") { 36 } else { 10 },
    margin=c("t" = 0.6, "r" = 0.5, "b" = 0.5, "l" = 0.3),
    set_theme = T
)
{
  require(ggplot2)
  t <- theme(axis.text.x = element_text(size=base_size, vjust=1),
             axis.text.y = element_text(size=base_size, hjust=1, vjust=.5),
             axis.title.x = element_text(size=base_size , vjust=0, hjust=0.5, face = "bold"),
             axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"),
             strip.text = element_text(size=base_size, color = "white"),
             strip.background = element_rect(fill = "black", color = "black"),
             legend.title = element_text(size=base_size, face = "bold", hjust= 0),
             legend.text = element_text(size=base_size),
             plot.margin = unit(margin, "lines"),
             aspect.ratio = 1)
  
  if (set_theme) theme_set(theme_bw(base_size=base_size) + t) else return(t)
}

## General functions ------------------------------------------------------------------
se <- function(x) sqrt(var(x) / (length(x) - 1))

scale_Gelman <- function(x) {
  (x - mean(x)) / (2 * sd(x))
}

my_unscale_Gelman <- function(d, scale_summary) {
  d %>%
    mutate(
      Item.Height = sItem.Height * 2 * scale_summary$Item.Height_sd + scale_summary$Item.Height_mean,
      Item.Backness = sItem.Backness * 2 * scale_summary$Item.Backness_sd + scale_summary$Item.Backness_mean)
}

geometric.mean = psych::geometric.mean

get_vowel_stats <-
  . %>%
  select(-F0_gm) %>%
  summarise(
    across(ends_with("gm"),
           .fns = list("mean" = mean, "var" = var)),
    F1F2_cov = cov(F1_gm, F2_gm), 
    F1F3_cov = cov(F1_gm, F3_gm),
    F2F3_cov = cov(F2_gm, F3_gm),
    heightbackness_cov = cov(height_gm, backness_gm),
    synthtalkerF0_gm = synthtalker_F0)

split_data <- function(
  data,
  proportion_data = .2
) {
  n_fold <- round(1 / proportion_data)
  
  data %>%
    group_by(Talker, category) %>%
    mutate(
      fold = base::sample(
        x = rep(1:n_fold, first(length(Talker)) %/% n_fold + 1), 
        size = first(length(Talker)), 
        replace = F))
}
  
## Normalization functions ------------------------------------------------------------
F0_to_SR = function(F0) {
  168 * (F0 / 168)^(1/3)
}

F1_to_height = function(F1, SR) {
  log(F1 / SR)
}

F2_to_backness = function(F1, F2) {
  log(F2 / F1)
}

height_to_F1 = function(height, SR) {
  exp(height) * SR
}

backness_to_F2 = function(backness, height, SR) {
  exp(backness) * height_to_F1(height, SR)
}

# Aliases
get_F1_Hz_from_height <- height_to_F1

get_F2_Hz_from_backness <- backness_to_F2

add_Hz_from_height_backness <- function(
  data,
  var.height = "Item.Height",
  var.backness = "Item.Backness",
  var.SR = "SR"
) {
  require(rlang)
  
  data %>%
    mutate(
      Item.Cue_Hz_F1 = height_to_F1(!! sym(var.height), !! sym(var.SR)),
      Item.Cue_Hz_F2 = backness_to_F2(!! sym(var.backness), !! sym(var.height), !! sym(var.SR)))
}

add_Mel_from_Hz <- function(
  data,
  var.F1 = "Item.Cue_Hz_F1",
  var.F2 = "Item.Cue_Hz_F2",
  var.F3 = "Item.Cue_Hz_F3"
) {
  require(rlang)
  
  data %>%
    mutate(
      Item.Cue_Mel_F1 = phonR::normMel(!! sym(var.F1)),
      Item.Cue_Mel_F2 = phonR::normMel(!! sym(var.F2)),
      Item.Cue_Mel_F3 = phonR::normMel(!! sym(var.F3)))
}

add_normalized <- function(
  data,
  var.cue.names = levels.cue.names
) {
  data %>%
    mutate_at(
      var.cue.names, 
      .funs = 
        c("r" = function(x) x,
          "c" = function(x) x - mean(x, na.rm = T),
          "s" = function(x) as.numeric(scale(x)))) %>%
    select(-(all_of(var.cue.names)))
}

get_transformation <- function(
  data,
  cues = c("F0","F1", "F2", "F3")
) {
  data %>%
    mutate_at(
      cues,
      .funs =
        c("Mel" = function(x) phonR::normMel(x),
          "Bark" = function(x) phonR::normBark(x),
          "ERB" = function(x) phonR::normErb(x),
          "semitones" = function(x) 12 * log2(x/100)))
  }

# Assumes that the two formants are called F1 and F2 and that F0 and F3 exists, too.
# (if Mel, Bark, or alike are meant to be normalized, then the F0-F3_* columns need to be renamed to F0-F3)
get_normalization_functions <- function(
    data, 
    normalize_based_on_fold_types = c("training")
) {
  message(paste("Making classic formant normalization functions based on data in", normalize_based_on_fold_types, "folds."))
  training_talker_statistics <-
    data %>%
    filter(fold_type %in% normalize_based_on_fold_types)

  training_talker_statistics %<>% 
    group_by(Talker) %>%
    summarise(
      formants_mean = list(c(mean(F0, na.rm = T), mean(F1, na.rm = T), mean(F2, na.rm = T), mean(F3, na.rm = T))),
      formants_min = list(c(min(F0, na.rm = T), min(F1, na.rm = T), min(F2, na.rm = T), min(F3, na.rm = T))),
      formants_max = list(c(max(F0, na.rm = T), max(F1, na.rm = T), max(F2, na.rm = T), max(F3, na.rm = T))),
      formants_sd = list(c(sd(F0, na.rm = T), sd(F1, na.rm = T), sd(F2, na.rm = T), sd(F3, na.rm = T))),
      formants_mean_log = list(c(mean(log(F0), na.rm = T), mean(log(F1), na.rm = T), mean(log(F2), na.rm = T), mean(log(F3), na.rm = T))),
      # For Nearey2 *both* formants are normalized by the same quantity: the mean of the three means of log-transformed F1-F3.
      # We thus make a two-element vector with the same normalization quantity (so that below we can apply it to both F1 & F2)
      formants_mean_mean_log = list(
        rep(sum(
          mean(log(F1), na.rm = T), 
          mean(log(F2), na.rm = T), 
          mean(log(F3), na.rm = T)) / 3, 2)))

  # use constants for normalization functions, when needed
  f <- function(newdata) {
    message("Applying classic formant normalization functionsto data.")
    newdata %<>%
      left_join(training_talker_statistics, by = "Talker") %>%
      mutate(
        formants = pmap(
          list(F0, F1, F2, F3), cbind),
        log_Nearey1 = map2(
          formants, formants_mean_log,
          ~ log(.x) - .y),
        log_Nearey2 = map2(
          formants, formants_mean_mean_log,
          ~ log(.x) - .y),
        Hz_Gerstman = pmap(
          list(formants, formants_min, formants_max),
          function(x, y, z) 999 * ((x - y) / (z - y))),
        Hz_Lobanov = pmap(
          list(formants, formants_mean, formants_sd), 
          function(x, y, z) (x - y) / z), 
        # Add Miller (since this is intrinsic, normalizing params are taken from newdata rather than training_talker_statistics)
        F0_gm = geometric.mean(F0),
        F0_log_Miller = log(F0),
        F1_log_Miller = F1_to_height(F1 = .data$F1, SR = 168 * (.data$F0_gm / 168) ^ (1/3)),
        F2_log_Miller = F2_to_backness(.data$F1, .data$F2),
        F3_log_Miller = log(.data$F3 / .data$F2),
        # Add Syrdal-Gopal's Bark-distance model (since this is intrinsic, normalizing params are taken from newdata rather than training_talker_statistics)
        F0_Bark_SyrdalGopal = F0_Bark,
        F0_Bark_SyrdalGopal2 = F0_Bark,
        F1_Bark_SyrdalGopal = .data$F1_Bark - .data$F0_Bark,
        F1_Bark_SyrdalGopal2 = .data$F1_Bark - .data$F0_Bark, 
        F2_Bark_SyrdalGopal = .data$F2_Bark - .data$F1_Bark,
        F2_Bark_SyrdalGopal2 = .data$F3_Bark - .data$F2_Bark) %>%
      mutate(
        across(
          c("log_Nearey1", "log_Nearey2", "Hz_Gerstman", "Hz_Lobanov"),
          list("F0" = ~ unlist(map(.x, function(x) x[1])),
               "F1" = ~ unlist(map(.x, function(x) x[2])),
               "F2" = ~ unlist(map(.x, function(x) x[3])),
               "F3" = ~ unlist(map(.x, function(x) x[4]))), 
          .names = "{.fn}_{.col}")) %>%
      select(-c(F0_gm, Hz_Lobanov, log_Nearey1, log_Nearey2, Hz_Gerstman))

    return(newdata)
  }
  
  return(f)
}

# For legacy use only
get_normalization <- get_normalization_functions

# Assumes that the two formants are called F1 and F2 and that F0 and F3 exists, too.
# (if Mel, Bark, or alike are meant to be normalized, then the F0-F3_* columns need to be renamed to F0-F3)
get_C_CuRE_function <- function(
    data, 
    cues = levels.cue.names.transform
) {
  data %<>% 
    group_by(Talker) %>%
    summarise(
      across(
        .cols = cues,
        .fns = list("overall_mean_for_CCuRE" = ~ mean(.x, na.rm = T)),
        .names = "{.fn}_{.col}"))

  f <- function(newdata) {
    require(glue)
    message(paste("Applying C-CuRE normalization to cues:", paste(cues, collapse = ",")))
    
    newdata %<>%
      left_join(data, by = "Talker") %>%
      mutate(
        across(
          .cols = cues,
          .fns = list("CCuRE" = ~ .x - get(glue("overall_mean_for_CCuRE_{cur_column()}"))),
          .names = "{.col}_{.fn}")) %>%
      select(-starts_with("overall_mean_for_CCuRE_"))
    
    return(newdata)
  }
  
  return(f)
}
  

# Keep around for legacy use but get rid of it once it's not used anymore (to avoid confusion)
add_C_CuRE <- function(
    data, 
    normalize_based_on_fold_types = c("training"), 
    cues = levels.cue.names.transform
) {
  message(paste("Making C-CuRE normalization functions based on data in", normalize_based_on_fold_types, "folds."))
  return(get_C_CuRE_function(data %>% filter(fold_type %in% normalize_based_on_fold_types), cues = cues))
}

## Outlier detection and correction --------------------------------------------------
get_cumulative_probability = function(x1, x2, mean, sigma) {
  pmvnorm(lower = -Inf, upper = c(x1, x2), mean = mean, sigma = sigma)
}

get_cumulative_probability_allCues = function(x1, x2, x3, x4, x5, mean, sigma) {
  pmvnorm(lower = -Inf, upper = c(x1, x2, x3, x4, x5), mean = mean, sigma = sigma)
}

get_cumulative_probability_univariate = function(x, mean, sd) {
  pnorm(x, mean = mean, sd = sd)
}

is_outlier = function(x, cutoff = outlier_probability_cutoff) {
  !between(
    x, 
    cutoff / 2, 
    1 - cutoff / 2)
}

obtain_densities <- . %>%
  group_by(Talker, category) %>%
  nest() %>%
  mutate(
    # density based on F1 and F2
    x_F1F2 = map(data, ~ cbind(.$F1, .$F2)),
    mean_F1F2 = map(x_F1F2, ~ colMeans(.x)),
    cov_F1F2 = map(x_F1F2, ~ cov(.x)),
    # density based on height and backness
    x_HeightBackness = map(data, ~ cbind(.$height, .$backness)),
    mean_HeightBackness = map(x_HeightBackness, ~ colMeans(.x)),
    cov_HeightBackness = map(x_HeightBackness, ~ cov(.x)),
    x_F1F2 = NULL,
    x_HeightBackness = NULL) %>%
  unnest(data) %>%
  # normalize densities within each talker and vowel
  mutate(
    cumulative_probability_F1F2 = pmap(
      .l = list(F1, F2, mean_F1F2, cov_F1F2), 
      .f = get_cumulative_probability),
    cumulative_probability_HeightBackness = pmap(
      .l = list(height, backness, mean_HeightBackness, cov_HeightBackness), 
      .f = get_cumulative_probability)) %>%
  mutate_at(
    vars(cumulative_probability_F1F2, cumulative_probability_HeightBackness),
    unlist) %>%
  ungroup()

obtain_densities_Norm <- . %>%
  group_by(Talker, category, Normalization.Type) %>%
    nest() %>%
    mutate(
      # density based on F1 and F2
      x_F1F2 = map(data, ~ cbind(.$F1, .$F2)),
      mean_F1F2 = map(x_F1F2, ~ colMeans(.x)),
      cov_F1F2 = map(x_F1F2, ~ cov(.x))) %>%
    unnest(data) %>%
    # normalize densities within each talker and vowel
    mutate(
      cumulative_probability_F1F2 = pmap(
        .l = list(F1, F2, mean_F1F2, cov_F1F2), 
        .f = get_cumulative_probability)) %>%
    mutate_at(
      vars(cumulative_probability_F1F2),
      unlist) %>%
    ungroup()

# obtain_densities_univariates <- function(
#   data,
#   cue = "cue"
# ) {
#   data %>%
#     group_by(Talker, Vowel) %>%
#     nest() %>%
#     mutate(
#       x_cue = map(data, ~cbind(!!! syms(cue))),
#       mean = map(x_cue, ~mean(.x, na.rm = T)),
#       sd = map(x_cue, ~sd(.x))) %>%
#     unnest(data) %>%
#     mutate(
#       "cumulative_probability_" = pmap(
#         .l = list(x, mean, sd),
#         .f = get_cumulative_probability_univariate)) %>%
#     mutate_at(
#       vars(starts_with("cumulative_probability")),
#       unlist) %>%
#     ungroup()
# }

#Obtain univariate densities for outlier correction of all cues in raw Hz
obtain_densities_univariates <- . %>%
  group_by(Talker, category) %>%
  nest() %>%
  mutate(
    # density based on F0
    x_F0 = map(data, ~ cbind(.$F0)),
    mean_F0 = map(x_F0, ~ mean(.x, na.rm = TRUE)),
    sd_F0 = map(x_F0, ~ sd(.x)),
    # density based on F1
    x_F1 = map(data, ~ cbind(.$F1)),
    mean_F1 = map(x_F1, ~ mean(.x)),
    sd_F1 = map(x_F1, ~ sd(.x)),
    # density based on F2
    x_F2 = map(data, ~ cbind(.$F2)),
    mean_F2 = map(x_F2, ~ mean(.x)),
    sd_F2 = map(x_F2, ~ sd(.x)),
    # density based on F3
    x_F3 = map(data, ~ cbind(.$F3)),
    mean_F3 = map(x_F3, ~ mean(.x)),
    sd_F3 = map(x_F3, ~ sd(.x)),
    # density based on Duration
    x_Duration = map(data, ~ cbind(.$Duration)),
    mean_Duration = map(x_Duration, ~ mean(.x)),
    sd_Duration = map(x_Duration, ~ sd(.x)),
    x_F0 = NULL,
    x_F1 = NULL,
    x_F2 = NULL,
    x_F3 = NULL,
    x_Duration = NULL) %>%
  unnest(data) %>%
  # normalize densities within each talker and vowel
  mutate(
    cumulative_probability_F0 = pmap(
      .l = list(F0, mean_F0, sd_F0), 
      .f = get_cumulative_probability_univariate),
    cumulative_probability_F1 = pmap(
      .l = list(F1, mean_F1, sd_F1), 
      .f = get_cumulative_probability_univariate),
    cumulative_probability_F2 = pmap(
      .l = list(F2, mean_F2, sd_F2),
      .f = get_cumulative_probability_univariate),
    cumulative_probability_F3 = pmap(
      .l = list(F3, mean_F3, sd_F3), 
      .f = get_cumulative_probability_univariate),
    cumulative_probability_F3 = pmap(
      .l = list(F3, mean_F3, sd_F3), 
      .f = get_cumulative_probability_univariate),
    cumulative_probability_Duration = pmap(
      .l = list(Duration, mean_Duration, sd_Duration), 
      .f = get_cumulative_probability_univariate)) %>%
  mutate_at(
    vars(starts_with("cumulative_probability")),
    unlist) %>%
  ungroup()

#Obtain multivariate densities for outlier correction of all cues in raw Hz
obtain_densities_allCues <- function(data) {
  data %>%
  group_by(Talker, category) %>%
  nest() %>%
  mutate(
    # density based on all cues
    x_cues = map(data, ~ cbind(.$F0, .$F1, .$F2, .$F3, .$Duration)),
    mean_cues = map(x_cues, ~ colMeans(.x)),
    cov_cues = map(x_cues, ~ cov(.x))) %>%
  unnest(data) %>%
  # normalize densities within each talker and vowel
  mutate(
    cumulative_probability_allCues = pmap(
      .l = list(F0, F1, F2, F3, Duration, mean_cues, cov_cues), 
      .f = get_cumulative_probability_allCues)) %>%
  mutate_at(
    vars(cumulative_probability_allCues),
    unlist) %>%
  ungroup()
}

# Vowel plot function
plot_vowels <- function(data, x, y) {
  ggplot(data,
        aes(
          x = F2_gm, 
          y = F1_gm,
          color = Vowel)) +
    geom_point(alpha = .5) +
    scale_x_reverse() +
    scale_y_reverse() +
    vowelPlot_components
}

## Ideal observer functions ---------------------------------------------------------

empirical_logit <- function(x, n = 100) return(log((x * n + .5) / (n - x * n + .5)))

get_likelihood <- function(x, mu, Sigma, ...) {
  if (is.null(dim(mu))) 
    return(dnorm(x, mu, Sigma, ...)) else { 
      if (max(dim(mu)) > 1) 
        return(dmvnorm(x, mu, Sigma, ...)) else
          return(dnorm(x, mu, Sigma, ...)) 
    } 
  
  return(NULL)
}

get_posterior <- function(
  data, 
  # names of column(s) with cues
  cues,
  # name of column with category
  category = "Vowel",
  # name(s) of column(s) that should be used for grouping ideal observers
  groups = "Condition",
  # name(s) of column with category means
  mu = "mu",
  # name of column with category covariance matrix
  Sigma = "Sigma"
) {
  # check that there is exactly one row per category 
  assert_that(any(is.data.frame(data), is_tibble(data)),
              msg = "data must be a data.frame or tibble.")
  assert_that(all(is.character(category), category %in% names(data)),
              msg = "category must be the name of a column in data")
  assert_that(all(is.character(groups), all(groups %in% names(data))),
              msg = "groups must be the name(s) of a column(s) in data")
  assert_that(all(c(mu, Sigma) %in% names(data)),
              msg = "mu and Sigma must be names of columns in data")
  assert_that(all(cues %in% names(data)), 
              msg = "all cues must be names of columns in data")
  assert_that(
    all(
      1 == data %>%
        group_by(!!! syms(groups), !! sym(category), !!! syms(cues)) %>%
        tally() %>%
        pull(n)), 
    msg = paste0(
      "This function expects one row per unique combination of group(s) (", 
      paste(groups, collapse = ", "), 
      "), category (", 
      category, 
      "), and cues (", 
      paste(cues, collapse = ", "), 
      ")."))
  
  # get posterior
  data %<>%
    mutate(x = pmap(list(!!! syms(cues)), cbind)) %>%
    mutate(
      # using logged likelihood to increase numerical precision (division through zero problem)
      Model.logLikelihood = unlist(pmap(list(x, !! sym(mu), !! sym(Sigma)), get_likelihood, log = T)),
      Model.Likelihood = exp(Model.logLikelihood)) %>%
    # Group by condition and each token, to get the relative likelihoods for the different vowels for that token
    group_by(!!! syms(groups), !!! syms(cues)) %>%
    # Just normalize each likelihood by the sum of all likelihoods for that condition and token 
    # (those likelihoods come from the different vowels all applied to the same token)
    mutate(Model.logPosterior = Model.logLikelihood - log(sum(exp(Model.logLikelihood)))) %>%
    ungroup() %>%
    mutate(
      Model.logPosterior = case_when(
        is.infinite(Model.logPosterior) & Model.logLikelihood == max(Model.logLikelihood) ~ 0,
        is.infinite(Model.logPosterior) & Model.logLikelihood != max(Model.logLikelihood) ~ min(Model.logPosterior),
        T ~ Model.logPosterior),
      Model.Posterior = exp(Model.logPosterior))
  
  return(data)
}

make_1dIO <- function(
  data,
  #  vowels = c("[i]", "[ɪ]", "[ɛ]", "[æ]", "[ɑ]", "[ʌ]", "[ʊ]", "[u]"),
  vowels = c("iy1", "ih1", "eh1", "ae1", "aa1", "ah1", "uh1", "uw1"),
  cues = "height",
  mean_string = "_gm_mean_mean",
  var_string = "_gm_var_mean",
  cov_string = "_cov_mean"
) {
  assert_that(any(is_tibble(data), is.data.frame(data)))
  assert_that(all(is.character(vowels), is_scalar_character(cues)))
  assert_that(all(is_scalar_character(mean_string), is_scalar_character(var_string)))
  
  data %>%
    filter(Vowel %in% vowels) %>%
    group_by(Vowel) %>%
    transmute(
      mu = !! sym(paste0(cues, mean_string)),
      Sigma = !! sym(paste0(cues, var_string)))
}

make_2dIO <- function(
  data,
  #  vowels = c("[i]", "[ɪ]", "[ɛ]", "[æ]", "[ɑ]", "[ʌ]", "[ʊ]", "[u]"),
  vowels = c("iy1", "ih1", "eh1", "ae1", "aa1", "ah1", "uh1", "uw1"),
  cues = c("height", "backness"),
  mean_string = "_gm_mean_mean",
  var_string = "_gm_var_mean",
  cov_string = "_cov_mean"
) {
  assert_that(any(is_tibble(data), is.data.frame(data)))
  assert_that(all(is.character(vowels), is.character(cues)))
  assert_that(all(is_scalar_character(mean_string), is_scalar_character(var_string)))
  
  data %>%
    filter(Vowel %in% vowels) %>%
    group_by(Vowel) %>%
    transmute(
      mu = pmap(
        list(!!! syms(paste0(cues, mean_string))), 
        cbind),
      Sigma = pmap(
        list(!!! syms(c(paste0(cues, var_string), paste0(paste(cues, collapse = ""), cov_string)))),
        .f = function(x, y, z) matrix(c(x, z, z, y), byrow = T, nrow = 2)))
}

# Changed function so that the cov column is defined when calling the function; otherwise issues with the paste cues function
make_2dIO_new <- function(
  data,
  #vowels = c("[i]", "[ɪ]", "[ɛ]", "[æ]", "[ɑ]", "[ʌ]", "[ʊ]", "[u]"),
  vowels = c("heed", "hid", "head", "had", "hud", "hood", "whod", "hod"),
  cues,
  mean_string = "_mean_mean",
  var_string = "_var_mean",
  cov
) {
  assert_that(any(is_tibble(data), is.data.frame(data)))
  assert_that(all(is.character(vowels), is.character(cues)))
  assert_that(all(is_scalar_character(mean_string), is_scalar_character(var_string)))
  
  data %>%
    filter(Vowel %in% vowels) %>%
    group_by(Vowel, Talker) %>%
    transmute(
      mu = pmap(
        list(!!! syms(paste0(cues, mean_string))), 
        cbind),
      Sigma = pmap(
        list(!!! syms(c(paste0(cues, var_string), cov))),
        .f = function(x, y, z) matrix(c(x, z, z, y), byrow = T, nrow = 2)))
}

make_3dIO <- function(
  data,
  #  vowels = c("[i]", "[ɪ]", "[ɛ]", "[æ]", "[ɑ]", "[ʌ]", "[ʊ]", "[u]"),
  vowels = c("iy1", "ih1", "eh1", "ae1", "aa1", "ah1", "uh1", "uw1"),
  cues,
  mean_string = "_gm_mean_mean",
  var_string = "_gm_var_mean",
  var1 = paste0(cues[1], "_gm_var_mean"),
  var2 = paste0(cues[2], "_gm_var_mean"),
  var3 = paste0(cues[3], "_gm_var_mean"),
  cov12 = paste0(paste(cues[c(1,2)], collapse = ""), "_cov_mean"),
  cov13 = paste0(paste(cues[c(1,3)], collapse = ""), "_cov_mean"),
  cov23 = paste0(paste(cues[c(2,3)], collapse = ""), "_cov_mean")
) {
  assert_that(any(is_tibble(data), is.data.frame(data)))
  assert_that(all(is.character(vowels), is.character(cues)))
  assert_that(all(is_scalar_character(mean_string), is_scalar_character(var_string)))
  
  data %>%
    filter(Vowel %in% vowels) %>%
    group_by(Vowel) %>%
    transmute(
      mu = pmap(
        list(!!! syms(paste0(cues, mean_string))), 
        cbind),
      Sigma = pmap(
        list(!!! syms(c(var1, var2, var3, cov12, cov23, cov13))),
        function(var1, var2, var3, cov12, cov23, cov13) {
          matrix(c(var1, cov12, cov13, cov12, var2, cov23, cov13, cov23, var3), byrow = T, nrow = 3)
        }
      ))
}

make_grid <- function(
  data.IOs,
  data.grid,
  #vowels = c("[i]", "[ɪ]", "[ɛ]", "[æ]", "[ɑ]", "[ʌ]", "[ʊ]", "[u]"),
  vowels = c("iy1", "ih1", "eh1", "ae1", "aa1", "ah1", "uh1", "uw1"),
  cues = c("height", "backness"),
  steps = 50L,
  mean_string = "_gm_mean_mean",
  var_string = "_gm_var_mean"
) {
  assert_that(any(is_tibble(data.IOs), is.data.frame(data.IOs)))
  assert_that(any(is_tibble(data.grid), is.data.frame(data.grid)))
  assert_that(all(is.character(vowels), is.character(cues)))
  assert_that(all(is_scalar_character(mean_string), is_scalar_character(var_string)))
  assert_that(is_scalar_integerish(steps))
  assert_that(all(vowels %in% unique(data.IOs %>% pull(Vowel))))
  for (c in cues) {
    assert_that(
      all(c(paste0(c, mean_string), paste0(c, var_string)) %in% names(data.grid)),
      msg = paste(
        "Mean and variance information for cue", 
        c, 
        "not found in", 
        as_name(data.grid)))
  }
  
  data.IOs %<>%
    filter(Vowel %in% vowels)
  
  for (c in cues) {
    data.IOs %<>%
      crossing(
        !! sym(c) := with(
          data.grid,
          seq(
            min(!! sym(paste0(c, mean_string)) - 2 * sqrt(!! sym(paste0(c, var_string)))),
            max(!! sym(paste0(c, mean_string)) + 2 * sqrt(!! sym(paste0(c, var_string)))),
            length.out = steps)))
  }
  
  data.IOs %>% 
    get_posterior(cues = cues)
}

make_grid_new <- function(
  data.IOs,
  data.grid,
  #vowels = c("[i]", "[ɪ]", "[ɛ]", "[æ]", "[ɑ]", "[ʌ]", "[ʊ]", "[u]"),
  vowels = c("heed", "hid", "head", "had", "hud", "hood", "whod", "hod"),
  cues,
  steps = 50L,
  mean_string = "_mean_mean",
  var_string = "_var_mean"
) {
  assert_that(any(is_tibble(data.IOs), is.data.frame(data.IOs)))
  assert_that(any(is_tibble(data.grid), is.data.frame(data.grid)))
  assert_that(all(is.character(vowels), is.character(cues)))
  assert_that(all(is_scalar_character(mean_string), is_scalar_character(var_string)))
  assert_that(is_scalar_integerish(steps))
  assert_that(all(vowels %in% unique(data.IOs %>% pull(IO.Vowel))))
  for (c in cues) {
    assert_that(
      all(c(paste0(c, mean_string), paste0(c, var_string)) %in% names(data.grid)),
      msg = paste(
        "Mean and variance information for cue", 
        c, 
        "not found in", 
        as_name(data.grid)))
  }
  
  data.IOs %<>%
    filter(IO.Vowel %in% vowels)
  
  for (c in cues) {
    data.IOs %<>%
      crossing(
        !! sym(c) := with(
          data.grid,
          seq(
            min(!! sym(paste0(c, mean_string)) - 2 * sqrt(!! sym(paste0(c, var_string)))),
            max(!! sym(paste0(c, mean_string)) + 2 * sqrt(!! sym(paste0(c, var_string)))),
            length.out = steps)))
  }
  
  data.IOs %>% 
    get_posterior(cues = cues)
}


plot_categorization_accuracy <- function(data, x, y) {
  ggplot(data,
         aes(
           x = factor(Item.CorrectResponse.Vowel, level = levels.response), 
           y = Model.Posterior,
           color = IO.Type)) +
    stat_summary(
      fun.data = mean_cl_boot,
      geom = "pointrange",
      position = position_dodge(.2)) +
    scale_x_discrete("Vowel of test token") +
    scale_y_continuous("Accuracy", limits = c(0,1)) +
    scale_color_discrete("Ideal observer") +
    facet_grid(. ~ Experiment, labeller = label_both)
}

## Experiment list functions ---------------------------------------------
make_catch_tibble <- function(
  .data,
  blocks, #start to final block where catch trials should be added (in the following format - 2:6)
  penultimate_block
  ) {
  .data %>%
  group_by(exposure_condition) %>%
  mutate(exposure_materials = sort(rep(c("A", "B"), length.out = n()))) %>%
  group_by(exposure_condition, exposure_materials) %>%
  mutate(block = sort(rep(blocks, length.out = n()))) %>%
  ungroup() %>%
  arrange(exposure_condition, exposure_materials, block) %>%
  crossing(exposure_list = 1:5) %>%
  mutate(block = (block + exposure_list) %% penultimate_block + 2) %>%
  mutate(block_order = case_when(exposure_list == 1 ~ "ABCDE",
                                 exposure_list == 2 ~ "BCDEA",
                                 exposure_list == 3 ~ "CDEAB",
                                 exposure_list == 4 ~ "DEABC",
                                 exposure_list == 5 ~ "EABCD"))
  }


## Data preparation ------------------------------------------------------- 
simplifyAnswer <- function(x) {
  x = gsub("[|]", ";", x)
  x = ifelse(str_ends(x, ";"), x, paste0(x, ";"))
  ifelse(str_count(x, ";") > 1, "multiple", str_remove(x, ";"))
}


formatData_NORM <- function(.data, experiment) {
  require(assertthat)
  require(lubridate)
  
  experiment_labels <- c("1a (synthesized)", "1b (natural)")
  assert_that(experiment %in% experiment_labels)

  n.stims <- case_when(
    experiment == experiment_labels[1] ~ 146 * 2 + 1,
    experiment == experiment_labels[2] ~ 72 * 2 + 1)
  levels.response <- case_when(
    experiment == experiment_labels[1] ~ levels.response,
    experiment == experiment_labels[2] ~ levels.response.natural)
  
  .data %<>%
    # Remove any variables that are all NA
    mutate_at(
      vars(
        # variables that are constant and INFORMATIVE across all rows
        hittypeid, hitgroupid, title, description, keywords, assignments, assignmentduration, autoapprovaldelay, reward,
        starts_with("Qualification"),
        # variables that are constant and UNinformative across all row
        assignmentstatus, hitstatus, reviewstatus, numavailable, numpending, numcomplete, annotation,
        # variables that are NAs across all rows
        assignmentapprovaltime, assignmentrejecttime, deadline, feedback, reject),  
      function(x) x <- NULL) %>%
    # Renaming
    rename(
      Experiment.Protocol = Answer.rsrb.protocol,
      List = Answer.list_test,
      AssignmentID = assignmentid,
      Assignment.Accept.DateTime.UTC = assignmentaccepttime,
      Assignment.Submit.DateTime.UTC = assignmentsubmittime,
      # Was mean to store user time in GMT format. Not helpful since it's not actually the local time of the user.
      # Assignment.Submit.DateTime.UserLocalTime = Answer.userDateTime, 
      Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC = Answer.userDateTimeOffset) %>%
    # Add user local time (for now ignoring day light savings etc.)
    mutate(
      Assignment.Submit.DateTime.UserLocalTime = Assignment.Submit.DateTime.UTC - minutes(Assignment.Submit.DateTime.UserLocalTime.OffsetFromUTC),
      Assignment.Submit.DuringDayTime = ifelse(between(hour(Assignment.Submit.DateTime.UserLocalTime), 7, 21), T, F)) %>%
    { if ("Answer.us.timezone" %in% names(.)) 
          rename(., Assignment.Submit.US_TimeZone = Answer.us.timezone) else 
            mutate(., Assignment.Submit.US_TimeZone = NA) } %>%
    # Separate the practice, exposure, and test columns into one column per trial
    # (each with one more parts then there are trials because the last element is also followed by a ";". 
    # This last empty element should then be removed. If you get a warning, however, that means that at 
    # least one participant has more trials than expected)
    { if (all(c("Answer.practResp") %in% names(.)))     
      separate(., 
               Answer.practResp,
               into = paste0("Practice_Trial", 1:3),
               sep = ";") else . } %>%
    { if ("Answer.exposureResp" %in% names(.)) 
      separate(.,
               Answer.exposureResp,
               into = paste0("Exposure_Trial", 1:101),
               sep = ";") else . } %>%
    separate(
      Answer.testResp,
      into = paste0("Test_Trial", 1:n.stims),
      sep = "\\;") %>%
    pivot_longer(
      cols = contains("_Trial"), 
      names_to = c("Phase", "Trial"),
      names_pattern = "(.*)_Trial(.*)"
    ) %>%
    # Remove empty final trial from each phase
    filter(value != "") %>%
    # Separate trial-level information into multiple columns
    separate(
      value,
      into = c("CHECK.Phase", "Trial.ProvideFeedback",
               "CHECK.Trial", "ItemID", 
               "Item.Filename", "Item.CorrectResponse",
               "Item.ImageOrder", "CHECK.Trial.ProvideFeedback",
               "Item.Repetitions", "Response",
               "Response.ClickPosition", "Response.ClickPosition.X", "Response.ClickPosition.Y",
               "Time.StartOfStimulus", "Time.EndOfTrial", "Response.RT"),
      sep = ",") %>%
    # Add Experiment information
    mutate(Experiment = experiment) %>%
    { if ("Answer.cond_exp" %in% names(.)) 
      rename(., Condition.Exposure = Answer.cond_exp) else 
        mutate(., Condition.Exposure = NA) } %>%
    rename_at(
      vars(starts_with("Answer.rsrb")), 
      ~ gsub("Answer\\.rsrb\\.([a-z])", "Participant\\.\\U\\1", .x, perl = T)) %>%
    # Make Trial numerical
    mutate(Trial = as.numeric(Trial)) %>%
    mutate(
      # Anonymize workers
      ParticipantID = as.numeric(factor(paste(Assignment.Submit.DateTime.UTC, workerid, AssignmentID))),
      # Extract item information
      REMOVE.Item = gsub("^(.*)\\.(wav)$", "\\1", Item.Filename),
      Item.Height = as.numeric(sapply(strsplit(REMOVE.Item, "_"), function(x) x[2])),
      Item.Backness = as.numeric(sapply(strsplit(REMOVE.Item, "_"), function(x) x[3])),
      Response = factor(Response, levels = levels.response),
      Response.Vowel = factor(
        plyr::mapvalues(Response, levels.response, levels.response.vowel),
        levels = levels.response.vowel)) %>%
    # Variable typing
    mutate_at(vars(CHECK.Trial, ItemID, Item.Repetitions, Response.ClickPosition.X, Response.ClickPosition.Y, Time.StartOfStimulus, Time.EndOfTrial, Response.RT),
              as.numeric) %>%
    mutate_at(vars(ParticipantID, workerid, Phase, Item.Filename, Item.CorrectResponse, Item.ImageOrder,
                   Response, Response.ClickPosition, Trial.ProvideFeedback, CHECK.Trial.ProvideFeedback,
                   hitid, AssignmentID),
              factor) %>%
    mutate(
      Participant.Sex = factor(Participant.Sex, levels.Sex),
      Participant.Race = factor(
        plyr::mapvalues(
          simplifyAnswer(Participant.Race), 
          c("amerind", "asian", "black", "multiple", "other", "white"),
          c("American Indican", "Asian", "Black", "multiple", "other", "White")), 
        levels.Race),
      Participant.Ethnicity = factor(
        plyr::mapvalues(
          simplifyAnswer(Participant.Ethnicity),
          c("Hisp", "NonHisp"),
          c("Hispanic", "Non-Hispanic")),
        levels.Ethnicity)) %>%
    # Get durational measures (in minutes)
    group_by(ParticipantID) %>%
    mutate(
      Duration.Assignment = difftime(Assignment.Submit.DateTime.UTC, Assignment.Accept.DateTime.UTC, units = "mins"),
      Duration.AllPhases = (max(Time.EndOfTrial) - min(Time.StartOfStimulus)) / 60000,
      ) %>%
    ungroup() %>%
    # Remove unnecessary columns and order remaining columns
    select(
      -starts_with("CHECK"),
      -starts_with("REMOVE")) %>%
    arrange(Experiment, ParticipantID, Phase, Trial) 
  
  return(.data)
}


sortVars_NORM <- function(.data) {
  .data %>% 
    relocate(
      Experiment,
      starts_with("Experiment."),
      List,
      ParticipantID,
      starts_with("Participant."), 
      starts_with("Condition"), 
      Phase, Trial,
      ItemID,
      starts_with("Item."),
      Response,
      starts_with("Response"),
      starts_with("Duration"),
      starts_with("Time"),
      starts_with("Assignment"),
      starts_with("Answer"),
      everything())
}


prepVars_NORM <- function(.data) {
  contrasts(.data$Item.ImageOrder) <- cbind("f vs. b" = c(-.5, .5))
  contrasts(.data$Experiment) <- cbind("1a (synthesized) vs. 1b (natural)" = c(.5, -.5))
  
  .data %<>% Gelman_scale_cues_NORM
  
  return(.data)
}


addExclusionSummary_NORM <- function(
  d, 
  exclude_based_on_catch_trials = T,
  plot = T, 
  return_data = T
) {
  d %<>%
    mutate(
      Exclude_Participant.Reason = factor(case_when(
        Exclude_Participant.because_of_TechnicalDifficulty ~ "Technical difficulty",
        Exclude_Participant.because_of_MultipleExperiments ~ "Repeat participant",
        Exclude_Participant.because_of_IgnoredInstructions ~ "No headphones",
        Exclude_Participant.because_of_RT ~ "Reaction time",
        Exclude_Participant.because_of_missing_trials ~ "Too many missing trials",
        T ~ "none"
      )))
  
    p <- d %>%
      group_by(Experiment, ParticipantID, Exclude_Participant.Reason, Assignment.Submit.DuringDayTime) %>%
      mutate(Response.log_RT = log10(Response.RT)) %>%
      summarise_at("Response.log_RT", .funs = list("mean" = mean, "sd" = sd), na.rm = T) %>%
      ggplot(aes(x = mean, y = sd)) +
      geom_point(aes(color = Exclude_Participant.Reason, shape = Exclude_Participant.Reason, alpha = Assignment.Submit.DuringDayTime)) +
      geom_rug() +
      scale_x_continuous("mean log-RT (in msec)") +
      scale_y_continuous("SD of log-RT") +
      scale_color_manual(
        "Reason for exclusion",
        breaks = c("none", 
                   "Technical difficulty", "Repeat participant", "No headphones", "Reaction time", "Too many missing trials"),
        values = c("black", rep("red", 8))) +
      scale_shape_manual(
        "Reason for exclusion",
        breaks = c("none", 
                   "Technical difficulty", "Repeat participant", "No headphones", "Reaction time", "Too many missing trials"),
        values = c(16, 15, 17, 10, 3, 4, 8, 9, 13)) +
      scale_alpha_manual(
        "Completed during\ndaytime (EST)?",
        breaks = c(T, F),
        values = c(1, .3))
    
  if (plot) { plot(p) }
  if (return_data) return(d) else return(p)
}


excludeData <- function(data) {
  data %<>%
    filter(
      Exclude_Participant.Reason == "none",
      !Exclude_Trial.because_of_RT)
  
  return(data)
}



## Data summaries ------------------------------------------------------- 

getParticipantsPerList_NORM <- function(d) {
  d %>%
    excludeData() %>%
    select(ParticipantID, List, hitid, starts_with("Condition.Exposure")) %>%
    distinct() %>%
    group_by(List) %>% 
    tally() %>%
    arrange(vars(everything()))
}


getMissingParticipantsPerList_NORM <- function(d, targetedParticipantsPerList) {
  d %>%
    excludeData() %>%
    select(ParticipantID, List, hitid, starts_with("Condition.Exposure")) %>%
    distinct() %>%
    group_by(List) %>% 
    tally() %>%
    mutate(n = targetedParticipantsPerList - n) %>%
    filter(n > 0) %>%
    arrange(vars(everything()))
}


plotDemographicInformation_NORM <- function(
  d, 
  rows = NULL,
  cols = NULL
) {
  p.answer <- d %>%
    group_by(Experiment, ParticipantID) %>%
    select(starts_with("Participant."), starts_with("Condition"), "Exclude_Participant.Reason") %>%
    summarize_all(first) %>%
    ggplot(aes(x = Participant.Age, fill = ifelse(Exclude_Participant.Reason == "none", "no", "yes"))) +
    scale_fill_manual("Excluded", values = c("black", "red")) +
    facet_grid(
      cols = vars(!!! syms(cols)), 
      rows = vars(!!! syms(rows)), 
      labeller = label_both)
  
  plot(p.answer + geom_histogram(color = NA, position = position_stack()) + xlab("reported age"))
  plot(p.answer + geom_bar() + aes(x = Participant.Sex) + xlab("reported sex"))
  plot(last_plot() + aes(x = Participant.Ethnicity) + 
         xlab("reported ethnicity") +
         theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  plot(last_plot() + aes(x = Participant.Race) +
    xlab("reported race")) 
}

## Model fitting and summarizing ----------------------------------------------------------------

fitModel <- function(
  .data,
  condition.exposure.pen, 
  condition.test.pen,
  override = F,
  model = NULL,
  priors = my_priors
) {
  require(brms)
  require(assertthat)
  
  assert_that(all(is.string(condition.exposure.pen), is.string(condition.test.pen), is.flag(override)))
  assert_that(condition.exposure.pen %in% c("H", "M"))
  assert_that(condition.test.pen %in% c("H", "M"))
  assert_that(xor(is.null(model), is.brmsfit(model)))
  
  filename = paste0("../models/Experiment-A-brm-test-PI", condition.exposure.pen, "-PI", condition.test.pen, ".rds")
  if (override & file.exists(filename)) file.remove(filename)
  
  .data %<>%
    filter(Experiment == "A", 
           Condition.Exposure.Pen == condition.exposure.pen, 
           Condition.Test.Pen == condition.test.pen) %>%
    droplevels() %>%
    prepVars()
  
  # Compile model unless another model has been handed to this function
  if (is.null(model)) {
    model <- brm(
      Response ~ 
        1 + 
        Condition.Exposure.LexicalLabel * 
        Block * cCondition.Test.Audio * 
        Condition.Test.OriginalLabel +
        (1 + Block * cCondition.Test.Audio * Condition.Test.OriginalLabel | ParticipantID) +
        (1 + Condition.Exposure.LexicalLabel * Block * cCondition.Test.Audio | ItemID),
      data = .data,
      family = brms::bernoulli(link = "logit"),
      prior = priors,
      warmup = 1000,
      iter = 3000,
      control = list(adapt_delta = .99),
      backend = "cmdstanr", threads = threading(2),
      file = filename)
  } else {
    model <- update(
      model,
      newdata = .data,
      warmup = 1000,
      iter = 3000,
      control = list(adapt_delta = .99),
      backend = "cmdstanr", threads = threading(2),
      file = filename)
  }
  
  return(model)
}


## Functions for formatting knitr output 
summarize_effect <- function(model, effect, parentheses = T) {
  h <- hypothesis(model, effect)$hypothesis
  op <- gsub("^.*(<|>|=).*$", "\\1", h$Hypothesis)
  
  m <- paste0("$\\hat{\\beta} = ", round(h$Estimate, 3), "; BF_{", op, "0} = ", round(h$Evid.Ratio, 1), "; p_{posterior} = ", round(h$Post.Prob, 2), "$")
  if (parentheses) m <- paste0("(", m, ")")
  
  return(m)
}


simplifyPredictorNames <- function(model) {
  # get all fixed effects predictors, parsed into their parts
  preds <- strsplit(gsub("^b_", "", grep("^b_", parnames(model), value = T)), ":") %>%
    # substitute 
    map(~gsub(".*Condition\\.([A-Za-z]+)\\.(Lexical|Original|Pen|Audio).*$", "\\2 \\(\\1\\)", .x)) %>%
    map(~gsub(" \\(Test\\)", "", .x)) %>%
    map(~paste(.x, collapse = " : ")) %>%
    unlist()
  
  # preds <- c(
  #   "Intercept",
  #   grep(":", preds[-1], invert = T, value = T,), 
  #   sort(grep(":", preds[-1], value = T)))
}





## Model visualization ------------------------------------------------------- 
get_scale_name <- function(variable) {
  case_when(
    variable == "Condition.Exposure.LexicalLabel" ~ "Shifted sound (exposure)",
    variable == "Condition.Exposure.Pen" ~ "Pen location (exposure)",
    variable %in% c("Condition.Test.Audio", "cCondition.Test.Audio") ~ "Acoustic continuum",
    variable == "Condition.Test.OriginalLabel" ~ "Visual label (test)",
    variable == "Condition.Test.Pen" ~ "Pen location (test)",
    T ~ variable
  )
}

get_scale_values <- function(variable, scale) {
  x <- case_when(
    variable == "Condition.Exposure.LexicalLabel" & scale %in% c("color", "colour", "fill") ~ colors.exposure.lexical_labels,
    variable == "Condition.Test.OriginalLabel" & scale %in% c("color", "colour", "fill") ~ colors.test.visual_labels,
    variable == "Condition.Test.Pen" & scale %in% c("color", "colour", "fill") ~ colors.test.pen_locations,
    variable == "Condition.Exposure.Pen" & scale %in% ("linetype") ~ as.character(linetypes.exposure.pen_locations),
    variable == "Condition.Test.OriginalLabel" & scale %in% ("linetype") ~ as.character(linetypes.test.visual_labels),
    variable == "Condition.Test.Pen" & scale %in% ("linetype") ~ as.character(linetypes.test.pen_locations),
    variable == "Condition.Exposure.Pen" & scale %in% ("shape") ~ as.character(shapes.exposure.pen_locations),
    variable == "Condition.Test.OriginalLabel" & scale %in% ("shape") ~ as.character(shapes.test.visual_labels),
    variable == "Condition.Test.Pen" & scale %in% ("shape") ~ as.character(shapes.test.pen_locations),
    T ~ NA_character_
  )
  
  if (scale %in% c("shape", "linetype")) x <- as.numeric(x)
  return(x)
}

get_continuum_breaks <- function(exp) {
  return(
    case_when(
      exp == "NORM A" ~ c(13,15,16,17,18,20),
      exp %in% c("A", "NORM B") ~ c(10,13,14,15,16,20),
      exp %in% c("NORM C") ~ c(12,17,18,19,20,24),
      T ~ NA_real_))
}

get_continuum_labels <- function(exp, sep = " ") {
  steps <- get_continuum_breaks(exp)
  return(paste(
    steps,
    c("most /s/-like", rep("", length(steps) - 2), "most /ʃ/-like"),
    sep = sep))
}

plot_test_predictions <- function(
  model,
  data = model$data,
  add_data = T,
  exp,
  predict_at = list(),
  mapping = aes(),
  facets = aes(rows = NULL, cols = NULL),
  breaks = list(
    Condition.Exposure.LexicalLabel = levels.exposure.lexical_labels,
    Condition.Exposure.Pen = levels.exposure.pen_locations,
    Block = 1:6,
    cCondition.Test.Audio = sort(unique(model$data$cCondition.Test.Audio)),
    Condition.Test.OriginalLabel = levels.test.visual_labels,
    Condition.Test.Pen = levels.test.pen_locations
  ),
  labels = list(
    Condition.Exposure.LexicalLabel = labels.exposure.lexical_labels,
    Condition.Exposure.Pen = labels.exposure.pen_locations,
    Block = 1:6,
    cCondition.Test.Audio = get_continuum_labels(exp),
    Condition.Test.OriginalLabel = labels.test.visual_labels,
    Condition.Test.Pen = labels.test.pen_locations
  ),
  response_scale = "linear",
  resolution_along_x = 51,
  ndraws = NULL,
  CIs = c(.95),
  alpha = .9
) {
  assert_that(!is.null(mapping[["x"]]), msg = "You must specify a mapping for the x-aesthetic.")
  
  unquoted_mapping <- map(mapping, ~ if (is_quosure(.x)) { as_name(.x) } else .x)
  unquoted_facets <- map(facets, ~ if (is_quosure(.x)) { as_name(.x) } else .x)
  mapped_predictors <- unique(c(as.character(unlist(unquoted_mapping)), as.character(unlist(unquoted_facets))))
  unmapped_predictors <- setdiff(names(data), c(mapped_predictors, "Response", "ParticipantID", "ItemID"))
  
  message(paste0("Plotting predictions for ", 
                 paste(mapped_predictors, collapse = " * "), 
                 ".\nPredictions will be generated for all existing values of predictors unless otherwise specified, as in the predict_at values for ", 
                 paste(names(predict_at), collapse = ", "),
                 ".\nPredictors that are neither mapped onto aesthetics nor specified in predict_at will be marginalized over."))
  
  d.grid <-
    data %>%
    data_grid(
      # add every variable that is not already specified in predict_at
      !!! syms(setdiff(c(mapped_predictors, unmapped_predictors), names(predict_at)))) %>%
    crossing(
      Block = predict_at[["Block"]],
      cCondition.Test.Audio = predict_at[["cCondition.Test.Audio"]])
  
  d.grid %<>%
    add_fitted_draws(
      model, 
      scale = "linear",
      n = ndraws, 
      value = "fitted", 
      re_formula = NA) %>% # set to NULL to include random effects (must then add them to data_grid), NA to ignore REs
    # average out unmapped predictors (I originally tried to do so before add_fitted_draws, but that requires making a
    # a choice for factor predictors as to which *level* to predict)
    group_by(!!! syms(mapped_predictors), .draw) %>%
    summarise(fitted = mean(fitted)) %>%
    { if (response_scale == "response") mutate(., fitted = plogis(fitted)) else . }
  
  # get labels for facets
  if (length(unquoted_facets) > 0) {
    my_row_labeller <- as_labeller(function(string) { 
      paste(
        get_scale_name(unquoted_facets[["rows"]]), 
        labels[[unquoted_facets[["rows"]]]][which(breaks[[unquoted_facets[["rows"]]]] == string)], 
        sep = ": ") })
    my_col_labeller <- as_labeller(function(string) { 
      paste(
        get_scale_name(unquoted_facets[["cols"]]), 
        labels[[unquoted_facets[["cols"]]]][which(breaks[[unquoted_facets[["cols"]]]] == string)], 
        sep = ": ") })
  }
  
  p <- 
    d.grid %>%
    ggplot(mapping = mapping) +
    # Facet if there's something to facet
    { if (length(unquoted_facets) > 0) facet_grid(
      rows = vars(!! facets[["rows"]]), 
      cols = vars(!! facets[["cols"]]), 
      labeller = labeller(
        .rows = my_row_labeller,
        .cols = my_col_labeller)) } +
    stat_lineribbon(mapping = aes(y = fitted), inherit.aes = T, .width = CIs, alpha = .9, ) +
    # Add the raw data?
    { if (add_data) 
      stat_summary(
        data = model$data %>%
          filter(if_all(
            names(predict_at), 
            ~ if (is.numeric(.x)) { 
              between(.x, min(predict_at[[cur_column()]]), max(predict_at[[cur_column()]])) 
            } else .x %in% predict_at[[cur_column()]])) %>%
          group_by(!!! syms(mapped_predictors), ParticipantID) %>%
          summarise(Response = mean(ifelse(Response == "ASHI", 1, 0))),
        mapping = aes(y = Response),
        fun.data = function(x) { 
          y <- mean_cl_boot(x)
          if (response_scale == "linear") y %<>% mutate_all(qlogis) 
          return(y) },
        geom = "pointrange", 
        position = position_dodge(.05)) } +
    # Set up x-axis
    scale_x_continuous(
      name = get_scale_name(unquoted_mapping[["x"]]),
      breaks = breaks[[unquoted_mapping[["x"]]]],                  
      labels = labels[[unquoted_mapping[["x"]]]],
      expand = c(0,0)) +
    scale_y_continuous(paste('Fitted', if (response_scale == "linear") "log-odds" else "proportion", 'of "ASHI" responses'))
  
  # Go through all aesthetics except for x and y (which has already been taken care of)
  if (!is.null(unquoted_mapping[["colour"]])) {
    p <- 
      p + 
      scale_color_manual(
        name = get_scale_name(unquoted_mapping[["colour"]]),
        breaks = breaks[[unquoted_mapping[["colour"]]]],
        labels = labels[[unquoted_mapping[["colour"]]]],
        values = get_scale_values(unquoted_mapping[["colour"]], scale = "color")) }
  if (!is.null(unquoted_mapping[["fill"]])) {
    p <- 
      p + 
      scale_fill_manual(
        name = get_scale_name(unquoted_mapping[["fill"]]),
        breaks = breaks[[unquoted_mapping[["fill"]]]],
        labels = labels[[unquoted_mapping[["fill"]]]],
        values = get_scale_values(unquoted_mapping[["fill"]], scale = "fill"))  
  } else p <- p + scale_fill_brewer(palette = "Greys")
  
  if (!is.null(unquoted_mapping[["shape"]])) { 
    p <- 
      p + 
      scale_shape_manual(
        name = get_scale_name(unquoted_mapping[["shape"]]),
        breaks = breaks[[unquoted_mapping[["shape"]]]],
        labels = labels[[unquoted_mapping[["shape"]]]],
        values = get_scale_values(unquoted_mapping[["shape"]], scale = "shape")) }
  if (!is.null(unquoted_mapping[["linetype"]])) {
    p <- 
      p + 
      scale_linetype_manual(
        name = get_scale_name(unquoted_mapping[["linetype"]]),
        breaks = breaks[[unquoted_mapping[["linetype"]]]],
        labels = labels[[unquoted_mapping[["linetype"]]]],
        values = get_scale_values(unquoted_mapping[["linetype"]], scale = "linetype")) }
  
  p <-  
    p +
    guides(linetype = guide_legend(override.aes = list(size = .6))) +
    theme(
      legend.position = "top", 
      legend.key.width = unit(1.25, "lines"), 
      aspect.ratio = .5) +
    { if (unquoted_mapping[["x"]] %in% c("cCondition.Test.Audio", "Condition.Test.Audio")) 
      theme(axis.text.x = element_text(angle = 33, hjust = 1)) }
  
  return(p)
}


plot_test_predictions_by_continuum <- function(
  model,
  data = model$data,
  predict_at = list("Block" = 1, "cCondition.Test.Audio" = seq_range(data$cCondition.Test.Audio, n = resolution_along_x)),
  mapping = aes(
    x = cCondition.Test.Audio, 
    color = Condition.Test.Pen,
    linetype = Condition.Test.OriginalLabel,
    shape = Condition.Test.OriginalLabel),
  resolution_along_x = 51,
  ...
) {
  plot_test_predictions(model, data, predict_at = predict_at, mapping = mapping, resolution_along_x = resolution_along_x, ...)
}

plot_test_predictions_by_block <- function(
  model,
  data = model$data,
  predict_at = list("Block" = seq_range(data$Block, n = resolution_along_x)),
  mapping = aes(
    x = cCondition.Test.Audio, 
    color = Condition.Test.Pen,
    linetype = Condition.Test.OriginalLabel,
    shape = Condition.Test.OriginalLabel),
  resolution_along_x = 51,
  ...
) {
  plot_test_predictions(model, data, predict_at = predict_at, mapping = mapping, resolution_along_x = resolution_along_x, ...)
}
