#' @title Emerging Infectious Network (EIN) Difficult Diagnoses survey data
#'
#' @description Responses to survey questions about advanced molecular
#' diagnostic testing for infectious diseases presented to medical personnel
#' who are members of EIN.
#'
#' @format ## `diagnoses`
#' A `list` with 3 objects that each have a length of 643.
#' \describe{
#'  \item{`wave`}{A `numeric` vector that lists of the data collect period when
#'  the responses were collected during.}
#'  \item{`q1`}{A `character` vector that lists the responses to the types of
#'  advanced molecular diagnostic tests that have been ordered or used. This is
#'  an example of responses based on select that all apply data collection
#'  process. Possible values are *Broad range 16S rRNA gene sequencing*
#'  (`Broadrange`), *Whole genome sequencing* (`Wholegenome`),
#' *Metagenomic next-generation sequencing* (`MNGS`), *None of the above*
#'  (`None`), and *Other* (`Other`), where the data values are listed within
#'  the parentheses. When multiple responses are present the data values are
#'  separated by `"|"`.}
#'  \item{`q2`}{A `factor` with 5 levels for the responses to how often within
#'  the past two years advanced molecular diagnostic testing was performed.
#'  Possible values are *Not at all*, *Once*, *Rarely*
#'  (quarterly or less often), *Sometimes* (monthly), and *Often* (weekly).
#'  This is an example of where the responses collected come from the multiple
#'  choice question.}
#' }
#'
#' @source [Difficult Diagnoses](https://ein.idsociety.org/surveys/survey/166/)
#' @usage data(diagnoses)
#' @examples
#' data(diagnoses)
#' str(diagnoses)
"diagnoses"

#' @title Emerging Infectious Network (EIN) Management of *S. aureus* Bacteremia
#' survey data
#'
#' @description Responses to survey questions about infectious disease
#' consultation for *staphylococcus aureus* bacteremia (SAB) presented to
#' medical personnel who are members of EIN.
#'
#' @details
#' Questions 1, 2, 3, 5, and 6 are clinical case study questions, where the
#' remaining questions ask about different treatment methodologies. Questions 1
#' and 2 provide a medical history in the case study, which is excluded in
#' the descriptions of the responses for `q1` and `q2a`. These questions are
#' asking what would be the treatment strategy of an infectious disease
#' physician given the full medical history of the patients.
#'
#' @format ## `bacteremia`
#' A `list` with 15 objects that each have a length of 669.
#' \describe{
#'  \item{`q1`}{A `character` vector that lists the responses to question 1
#'  case study.}
#'  \item{`q2`}{A `character` vector that lists the responses to question 2
#'  case study.}
#'  \item{`q3`}{A `character` vector that lists the responses to preferred
#'  choice of valve imaging in a patient with MSSA bacteremia.}
#'  \item{`q4`}{A `character` vector that lists the responses on how to treat
#'  a clinically stable patient that has MRSA bacteremia.}
#'  \item{`q5`}{A `character` vector that lists the responses on
#'  antibiotic treatment strategy for a patient with MRSA endocarditis on day 6
#'  of vancomycin with persistent positive blood cultures.}
#'  \item{`q6`}{A `character` vector that lists the responses to the treatment
#'  course for a 35 y/o previously healthy man with left-sided MSSA endocarditis
#'  but no meningitis or CNS disease.}
#'  \item{`q7_bc`}{A `character` vector that lists the responses to how often
#'  repeat blood cultures from 24 hours to 48 hours until negative is used when
#'  evaluating a patient with SAB.}
#'  \item{`q7_echo`}{A `character` vector that lists the responses to how often
#'  a TTE and/or TEE is used when evaluating a patient with SAB.}
#'  \item{`q7_spine`}{A `character` vector that lists the responses to how often
#'  a MRI of the spine is used when evaluating a patient with SAB.}
#'  \item{`q7_abd`}{A `character` vector that lists the responses to how often
#'  a CT of the abdominal/pelvic area is used when evaluating a patient with
#'  SAB.}
#'  \item{`q7_eye`}{A `character` vector that lists the responses to how often
#'  a dilated eye exam is used when evaluating a patient with SAB.}
#'  \item{`q9`}{A `character` vector that lists the responses to dosage of
#'  daptomycin for the treatment of MRSA bacteremia.}
#'  \item{`q10`}{A `character` vector that lists the responses to management
#'  strategy and decision regarding anticoagulation on patients with MSSA
#'  bacteremia and a deep vein thrombus at the site of a recently removed PICC
#'  line.}
#'  \item{`q8`}{A `data.frame` of 7 `character` variables that indicate whether
#'  response resulted in the selection of the *select all apply* response item.
#'  The responses are focused on what conditions would increase the treatment
#'  duration from 2 weeks to 4-6 weeks assuming that a TTE and/or TEE are
#'  negative in a patient with MRSA bacteremia. If the response item was
#'  selected a value of `Yes` is reported while `No` indicates the condition was
#'  not selected.}
#'  \item{`wave`}{A `numeric` vector that lists of the data collect period when
#'  the responses were collected during.}
#' }
#'
#' @source [S. aureus Bacteremia](https://ein.idsociety.org/surveys/survey/99/)
#' @usage data(bacteremia)
#' @examples
#' data(bacteremia)
#' str(bacteremia)
"bacteremia"
