#' Data from 'Preventing Sexual Violence -- A behavioral Problem Without a Behaviorally-Informed Solution'
#'
#' A dataset of primary prevention studies aimed at reducing sexual violence.
#' Each record includes details about the study's methods, intervention specifics, participant demographics,
#' outcome measures, design and more.
#' @format A data frame with 489 rows and 65 columns.
#' @source Porat et al. (2024). "Preventing Sexual Violence -- A Behavioral Problem Without a Behaviorally-Informed Solution"
#' \describe{
#'   \item{author}{Last name of the paper's first author.}
#'   \item{year}{Year the study was published.}
#'   \item{intervention_name}{Name of the intervention used in the study.}
#'   \item{scale_name}{Name of the scale used to measure outcomes.}
#'   \item{delay}{Time in days between the intervention and outcome measurement.}
#'   \item{paper_title}{Full title of the paper.}
#'   \item{lab_field}{Study setting categorized as 'lab' or 'field'.}
#'   \item{program_name}{Name of the program under study.}
#'   \item{publication_type}{Type of publication, e.g., 'published', 'dissertation'.}
#'   \item{origin}{Source of data inclusion, such as 'DeGue' or 'Online Search'.}
#'   \item{setting}{Primary setting of the study, e.g., 'college', 'community'.}
#'   \item{setting2}{Secondary setting if applicable.}
#'   \item{stage_in_life}{Participant's life stage, e.g., 'college students'.}
#'   \item{mean_age}{Mean age of participants.}
#'   \item{participant_sex}{Sex composition of participants.}
#'   \item{percent_of_male}{Percentage of male participants.}
#'   \item{percent_dominant_racial_or_ethnic_group}{Dominant racial or ethnic group percentage.}
#'   \item{country}{Country where the study was conducted.}
#'   \item{state}{US state where the study was conducted, if applicable.}
#'   \item{number_of_participant}{Total number of participants.}
#'   \item{type_of_control}{Type of control group used.}
#'   \item{intervention_description}{Indicator if the intervention is described.}
#'   \item{brief_description_of_the_intervention}{Short description of the intervention.}
#'   \item{delivery_of_treatment}{How the treatment was delivered.}
#'   \item{intervention_length}{Duration of the intervention in days.}
#'   \item{time_intervention_took}{Total duration of the intervention in minutes.}
#'   \item{additional_contact}{Indicates additional contact with participants outside sessions.}
#'   \item{stated_purpose_of_the_intervention}{Codes representing the intervention's purposes.}
#'   \item{intervention_description.1}{Additional description of the intervention, if any.}
#'   \item{stated_purpose}{Stated purposes of the study.}
#'   \item{condition_gender}{Gender condition used in the study.}
#'   \item{n_items}{Number of items in the outcome measure.}
#'   \item{contains_randomization}{Indicates if randomization was used.}
#'   \item{study_design}{Design of the study, such as 'Randomized Controlled Trial'.}
#'   \item{is_primary}{Indicates if the outcome was the primary variable.}
#'   \item{scale_type}{Type of outcome scale, e.g., 'ideas', 'behavior'.}
#'   \item{type_of_at_risk_population}{Description of any at-risk population noted.}
#'   \item{n_conditions}{Number of conditions in the study.}
#'   \item{n_c_pre}{Control participants at pretest.}
#'   \item{n_c_post}{Control participants at posttest.}
#'   \item{n_t_pre}{Treatment participants at pretest.}
#'   \item{n_t_post}{Treatment participants at posttest.}
#'   \item{n_t_group}{Number of treatment groups.}
#'   \item{n_c_group}{Number of control groups.}
#'   \item{cluster_type}{Type of cluster used in the study.}
#'   \item{eff_type}{Type of effect size calculated.}
#'   \item{u_s_d}{Unstandardized effect size.}
#'   \item{anticipated_direction}{Expected direction of the effect.}
#'   \item{ctrl_sd}{Standard deviation of the control group.}
#'   \item{unique_paper_id}{Unique identifier for each paper.}
#'   \item{unique_study_id}{Unique identifier for each study.}
#'   \item{robust_quasi_check}{robustness check for re-classifying some quasi-experimental designs as experimental}
#'   \item{behavior_type}{Type of behavior measured.}
#'   \item{decade}{Decade in which the study was published.}
#'   \item{total_n}{Total number of participants post-intervention.}
#'   \item{quantile_var}{Quantile variable based on total sample size.}
#'   \item{labels}{Concatenated labels of author and year for use in plots.}
#'   \item{ra}{Indicates if the study had random assignment.}
#'   \item{yes_delay}{Indicates if there was a delay in measurement.}
#'   \item{published_unpublished}{Publication status of the study.}
#'   \item{has_both}{Whether the study measures both ideas and behaviors.}
#'   \item{ideas_behaviors}{Categorization of measured outcomes.}
#'   \item{d}{Standardized effect size (Delta).}
#'   \item{var_d}{Variance of the effect size.}
#'   \item{se_d}{Standard error of the effect size.}
#' }
#' @keywords dataset

"sv_data"
