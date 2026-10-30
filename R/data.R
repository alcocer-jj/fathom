#' UK parliamentary sentences, coded for sentiment on a six-level scale
#'
#' A hundred sentences from ParlaSent 1.0 (Mochtak et al. 2023), the English
#' training file, drawn from UK parliamentary proceedings and annotated by two
#' coders with a reconciliation pass. It is the package's exclusive-choice
#' corpus and the only one whose labels collide.
#'
#' Normalized for scoring, `Negative` and `Neutral` both begin with N. Prefix
#' matching cannot tell them apart, since a split label often emits only its
#' first character, so [instrument()] refuses to build this label set that way
#' and it can only be scored with `exact = TRUE` and a live backend. The
#' collision is a property of a published coding scheme rather than something
#' constructed to demonstrate one.
#'
#' The annotators worked six levels and the three-level `label` is their
#' collapse. Both mixed levels fold to their own polarity, so `Neutral` means no
#' sentiment content and nothing else. Ambiguity lives in `M_Positive` and
#' `M_Negative`, which are inside the positive and negative classes rather than
#' beside them.
#'
#' | level | meaning |
#' | :-- | :-- |
#' | `Positive` | entirely or predominantly positive |
#' | `M_Positive` | ambiguous or mixed, leaning positive |
#' | `P_Neutral` | no sentiment content, leaning positive |
#' | `N_Neutral` | no sentiment content, leaning negative |
#' | `M_Negative` | ambiguous or mixed, leaning negative |
#' | `Negative` | entirely or predominantly negative |
#'
#' @section Sampling:
#' Stratified on the six levels rather than on the three, seed 444. Seventeen
#' each of `Positive`, `Negative`, `P_Neutral`, and `N_Neutral`, and sixteen
#' each of the two mixed levels, giving 33 negative, 34 neutral, and 33
#' positive.
#'
#' Sampling on the collapse would have let the draw decide the mix inside each
#' class, and nothing would record which mix arrived. The neutral levels matter
#' most. `P_Neutral` outnumbers `N_Neutral` 413 to 267 in the source, so a
#' proportional draw would give a neutral class that leans positive, and a model
#' answering that lean would look biased when the sample was.
#'
#' The mixed levels are kept rather than dropped. They are the only sentences
#' that give `margin` and `prob_other` anything to separate. A corpus of
#' unambiguous cases returns saturated on every row and every uncertainty
#' diagnostic has nothing to show.
#'
#' @section Class balance:
#' The source file is 48.8 percent negative, 26.2 percent neutral, and 25.0
#' percent positive, against 33, 34, and 33 here. Recall, specificity, and AUC
#' are computed within a class and carry over. Accuracy, precision, F1, and
#' kappa move with the class mix and do not. `weight3` reweights to the source
#' proportions, and `weight` does the same at the six-level stratum, though at a
#' higher cost in precision. The effective sample size is about 90 using
#' `weight3` and about 69 using `weight`.
#'
#' The sample is also deliberately harder than the file it came from. Mixed
#' levels are 32 percent here against 14.6 percent there.
#'
#' @section The human ceiling:
#' `annotator1` and `annotator2` are the two independent codings before
#' reconciliation, so the corpus carries its own benchmark. On these hundred
#' sentences the two coders agree with each other on 0.720, which is 0.765 on
#' the four unambiguous levels, 0.625 on the two mixed ones, and 0.570 on the
#' raw six. Across the full 2,600-sentence file they agree on 0.783, and on the
#' raw six levels 0.660.
#'
#' A model is worth reading against 0.720 rather than against 1. The gap between
#' the pure and mixed figures is what the uncertainty diagnostics should track.
#'
#' @format A tibble with 100 rows and 19 columns.
#'
#' - `id` is the document identifier, `uk_001` to `uk_100`.
#' - `sentence` is the document, 11 to 32 words, median 22.
#' - `label` is the three-level reconciled sentiment. Factor, `Negative`,
#'   `Neutral`, `Positive`.
#' - `reconciliation` is the six-level reconciled code. Factor, ordered from
#'   `Negative` through `Positive`.
#' - `annotator1` and `annotator2` are the two independent six-level codings.
#' - `name` is the member speaking. 93 distinct across the hundred.
#' - `party` is the party affiliation. Eight parties.
#' - `gender` is the recorded gender of the member.
#' - `ruling` says whether the member sat with the government or the opposition.
#' - `date` is the date of the sitting, 2015-01-08 to 2021-02-23.
#' - `term` is the parliamentary term.
#' - `document_id` and `sentence_id` are ParlaSent internal identifiers.
#' - `split` is the authors' own train, dev, and test partition. This sample
#'   crosses all three, since nothing here is trained.
#' - `stratum_N` is the size of the six-level stratum in the source file.
#' - `stratum_n` is the number drawn from it.
#' - `weight` is `stratum_N / stratum_n`, for weighting back at the six-level
#'   stratum.
#' - `weight3` is the same pooled to the three-level class.
#' @source Mochtak, M., Rupnik, P., Meden, K., and Ljubesic, N. (2023). The
#'   multilingual sentiment dataset of parliamentary debates ParlaSent 1.0.
#'   Slovenian language resource repository CLARIN.SI.
#'   \url{http://hdl.handle.net/11356/1868}
#'
#'   Distributed under CC BY-SA 4.0, unlike the rest of this package. See
#'   `LICENSE.note`. Drawn from the English training file with seed 444.
#' @references
#'   Kuzman Pungersek, T., Ljubesic, N., Erjavec, T., et al. (2025).
#'   Linguistically annotated multilingual comparable corpora of parliamentary
#'   debates ParlaMint-en.ana 5.0. CLARIN.SI.
#'   \url{http://hdl.handle.net/11356/2006}
#'
#'   Mochtak, M., Rupnik, P., and Ljubesic, N. (2024). The ParlaSent
#'   multilingual training dataset for sentiment identification in parliamentary
#'   proceedings. \emph{Proceedings of LREC-COLING 2024}, 16024-16036.
#' @examples
#' data(parlasent)
#' table(parlasent$reconciliation, parlasent$label)
#'
#' # The two coders before reconciliation, on the raw six levels.
#' mean(parlasent$annotator1 == parlasent$annotator2)
"parlasent"

#' Mexican access-to-information requests
#'
#' Three hundred requests filed through Mexico's INFOMEX transparency system,
#' drawn from a corpus Erlich et al. hand-coded with six Mexico City research
#' assistants over ten weeks. Their codings are not shipped here. What ships is
#' the request text and the four fields that trace one back to its source,
#' which is what a run with no training data needs. The coded variables are in
#' the replication materials named under `@source`, but the codebook was never
#' published and the supplemental material names them without saying how they
#' were defined, so what arrives is column names. The requests are in Spanish
#' and run from a single word to 588, median 42.
#'
#' @section The text:
#' `text` is what the requester typed. Commas were stripped when the source was
#' written to CSV, which is not recoverable and shows as double spaces.
#'
#' @format A tibble with 300 rows and 5 columns.
#'
#' - `id` is the row identifier from the source data.
#' - `folio` is the INFOMEX request number, which traces back to the source.
#' - `date` is the date of the request.
#' - `dependency` is the agency the request was filed with.
#' - `text` is the request as filed, in Spanish.
#' @source
#' Erlich, A., S. G. Dantas, B. E. Bagozzi, D. Berliner, and B. Palmer-Rubin.
#' 2022. Multi-label Prediction for Political Text-as-Data.
#' \emph{Political Analysis} 30(4), 463--480. \doi{10.1017/pan.2021.15}
#'
#' Replication data at \doi{10.7910/DVN/SOVPA4}, released under CC0 1.0.
#' Sampled with seed 444. The underlying requests are published by Mexico's
#' federal transparency system.
#' @examples
#' data(ati)
"ati"
