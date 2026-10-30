# Stem presets for template_items() and exclusive instruments.
#
# Structural rather than substantive. "mention" fires on a passing reference,
# "about" requires the text to be centrally concerned with the category.
#
# Category sets are not shipped. Competing schemes are different constructs with
# different literatures, so category sets belong in the vignettes with
# citations.
# Question stems for independent binary items, one set per medium. The sets are
# not translations of one another. A picture does not mention anything, so
# `image` carries `show` and `depict` in place of `topic` and `mention`, and a
# video carries both because a row holds frames and a transcript at once and
# the verb is what says which one the question is about.
#
# Spanish is written rather than converted. The demonstrative agrees with the
# noun, `esta imagen` against `este video`, which is why a `{noun}` slot was
# rejected. `video` is unaccented, which is the Latin American form and the one
# the `ati` corpus already implies.
ITEM_STEMS <- list(
  en = list(
    text = list(
      topic = "Does this text discuss {label}?",
      mention = "Does this text mention {label}?",
      express = "Does this text express {label}?",
      about = "Is this text primarily about {label}?",
      frame = "Does this text frame the issue in terms of {label}?"
    ),
    audio = list(
      topic = "Does this recording discuss {label}?",
      mention = "Does this recording mention {label}?",
      express = "Does this recording express {label}?",
      about = "Is this recording primarily about {label}?",
      frame = "Does this recording frame the issue in terms of {label}?"
    ),
    image = list(
      show = "Does this image show {label}?",
      depict = "Does this image depict {label}?",
      express = "Does this image express {label}?",
      about = "Is this image primarily about {label}?",
      frame = "Does this image frame the issue in terms of {label}?"
    ),
    video = list(
      show = "Does this video show {label}?",
      depict = "Does this video depict {label}?",
      discuss = "Does this video discuss {label}?",
      express = "Does this video express {label}?",
      about = "Is this video primarily about {label}?",
      frame = "Does this video frame the issue in terms of {label}?"
    )
  ),
  es = list(
    text = list(
      topic = "\u00bfEste texto trata sobre {label}?",
      mention = "\u00bfEste texto menciona {label}?",
      express = "\u00bfEste texto expresa {label}?",
      about = "\u00bfEste texto trata principalmente de {label}?",
      frame = paste0(
        "\u00bfEste texto enmarca el tema en t\u00e9rminos de {label}?"
      )
    ),
    audio = list(
      topic = "\u00bfEsta grabaci\u00f3n trata sobre {label}?",
      mention = "\u00bfEsta grabaci\u00f3n menciona {label}?",
      express = "\u00bfEsta grabaci\u00f3n expresa {label}?",
      about = "\u00bfEsta grabaci\u00f3n trata principalmente de {label}?",
      frame = paste0(
        "\u00bfEsta grabaci\u00f3n enmarca el tema en t\u00e9rminos de ",
        "{label}?"
      )
    ),
    image = list(
      show = "\u00bfEsta imagen muestra {label}?",
      depict = "\u00bfEsta imagen representa {label}?",
      express = "\u00bfEsta imagen expresa {label}?",
      about = "\u00bfEsta imagen trata principalmente de {label}?",
      frame = paste0(
        "\u00bfEsta imagen enmarca el tema en t\u00e9rminos de {label}?"
      )
    ),
    video = list(
      show = "\u00bfEste video muestra {label}?",
      depict = "\u00bfEste video representa {label}?",
      discuss = "\u00bfEste video trata sobre {label}?",
      express = "\u00bfEste video expresa {label}?",
      about = "\u00bfEste video trata principalmente de {label}?",
      frame = paste0(
        "\u00bfEste video enmarca el tema en t\u00e9rminos de {label}?"
      )
    )
  )
)

# Question stems for exclusive instruments. The question is always which one,
# and the rest of the prompt is the option listing plus the cue.
CHOICE_STEMS <- list(
  en = list(
    text = list(
      primary = "What is the PRIMARY subject of this text?",
      best = "Which category best describes this text?"
    ),
    audio = list(
      primary = "What is the PRIMARY subject of this recording?",
      best = "Which category best describes this recording?"
    ),
    image = list(
      primary = "What is the PRIMARY subject of this image?",
      best = "Which category best describes this image?"
    ),
    video = list(
      primary = "What is the PRIMARY subject of this video?",
      best = "Which category best describes this video?"
    )
  ),
  es = list(
    text = list(
      primary = "\u00bfCu\u00e1l es el tema PRINCIPAL de este texto?",
      best = "\u00bfQu\u00e9 categor\u00eda describe mejor este texto?"
    ),
    audio = list(
      primary = paste0(
        "\u00bfCu\u00e1l es el tema PRINCIPAL de esta grabaci\u00f3n?"
      ),
      best = paste0(
        "\u00bfQu\u00e9 categor\u00eda describe mejor esta grabaci\u00f3n?"
      )
    ),
    image = list(
      primary = "\u00bfCu\u00e1l es el tema PRINCIPAL de esta imagen?",
      best = "\u00bfQu\u00e9 categor\u00eda describe mejor esta imagen?"
    ),
    video = list(
      primary = "\u00bfCu\u00e1l es el tema PRINCIPAL de este video?",
      best = "\u00bfQu\u00e9 categor\u00eda describe mejor este video?"
    )
  )
)

# Every medium a stem set exists for, in the order the refusals list them.
STEM_MEDIA <- c("text", "audio", "image", "video")

#' The stem for a preset name, or NULL
#'
#' `medium` carries no default because the sets are not translations of one
#' another. `image` has no `mention` and `video` has a `discuss` nothing else
#' has, so a default would answer a question about the document with a guess.
#'
#' It is only ever consulted for a preset name. A literal question carries its
#' own wording and needs none, which is what keeps every existing call working.
#'
#' @param tab A stem table, `ITEM_STEMS` or `CHOICE_STEMS`.
#' @param lang,medium,key As passed by the caller.
#' @param what A string giving the argument name to use in the refusal.
#' @return A single string, or NULL where the key names no preset.
#' @keywords internal
#' @noRd
stem_lookup <- function(tab, lang, medium, key, what) {
  known <- unique(unlist(lapply(tab[[lang]], names)))
  if (!key %in% known) {
    return(NULL)
  }
  if (is.null(medium)) {
    stop(
      fmt_err(
        paste0(
          "`",
          what,
          " = \"",
          key,
          "\"` names a preset, so `medium` is ",
          "required. The sets differ by medium rather than translating, so ",
          "there is nothing to fall back on."
        ),
        paste0("One of ", paste(STEM_MEDIA, collapse = ", "), ".")
      ),
      call. = FALSE
    )
  }
  if (!medium %in% STEM_MEDIA) {
    stop(
      fmt_err(paste0(
        "`medium` must be one of ",
        paste(STEM_MEDIA, collapse = ", "),
        ", not \"",
        medium,
        "\"."
      )),
      call. = FALSE
    )
  }
  out <- tab[[lang]][[medium]][[key]]
  if (is.null(out)) {
    stop(
      fmt_err(
        paste0(
          "`",
          what,
          " = \"",
          key,
          "\"` is not a preset for medium \"",
          medium,
          "\"."
        ),
        paste0(
          "Available: ",
          paste(names(tab[[lang]][[medium]]), collapse = ", "),
          "."
        )
      ),
      call. = FALSE
    )
  }
  out
}

# Language presets for prompt scaffolding. For any other language, pass cue,
# text_label, and answer_label explicitly. One cue serves both kinds of
# instrument, and `or_word` is the conjunction paste_or() puts before the last
# label.
LANG_PRESETS <- list(
  en = list(
    text_label = "Text",
    answer_label = "Answer",
    or_word = "or",
    cue_fmt = "\n\nAnswer with exactly one word: %s."
  ),
  es = list(
    text_label = "Texto",
    answer_label = "Respuesta",
    or_word = "o",
    cue_fmt = "\n\nResponde con exactamente una palabra: %s."
  )
)

#' Construct a validated instrument of items
#'
#' @description
#' Bundles the questions to be answered, the labels the model may answer with,
#' and the instruction that elicits them.
#'
#' @section Label matching:
#' Scoring reads one token and counts it as a label when it opens that label, so
#' `BU` counts as BUDGET. Nothing has to be discovered first, because a model
#' writing BUDGET must emit a prefix of it, and that follows from every
#' tokenizer in use detokenizing by concatenation rather than from any one
#' model's vocabulary.
#'
#' The price is that labels must differ at their first character. Under prefix
#' matching this is checked at construction from the strings alone, with no
#' backend. A split label frequently emits only its first character, and two
#' labels sharing one cannot then be told apart. With distinct first characters
#' no two labels share any prefix, so every match resolves to one label.
#'
#' A label carrying whitespace is refused at construction, because scoring reads
#' one token. One token cannot span a space, so a two-word label is matched by
#' its opening word and the rest is never read, which scores the opening word
#' rather than scoring zero. Join the words if the answer really is compound,
#' since `NOT_SURE` and `NOT-SURE` are both accepted.
#'
#' `exact = TRUE` is the way out when they have to collide. Each label is probed
#' once, the emitted token is recorded, and scoring consults that record instead
#' of reading a prefix. Passing a `backend` alone runs the probe without
#' changing how scoring works, which is worth doing once on a new model to see
#' the token map.
#'
#' @section Labels and definitions:
#' Labels and definitions are different things. `labels` names the answer words
#' and the surface forms they may appear in. `definitions` holds the text a
#' reader or a model needs to know what a label means. Keeping them apart
#' matters because scoring matches the token at position 1 against `labels`, so
#' a definition sitting in `labels` registers a token the model never emits and
#' that label scores zero on every document with no error raised.
#'
#' An exclusive instrument lists the definitions under its question, which is
#' what lets a category be a long phrase while the answer stays one word. Other
#' instruments store them without putting them in the prompt, since their
#' question is yours to write.
#'
#' @section Ordinal labels:
#' `scores` attaches a numeric value to each label. The values are metadata.
#' They never appear in the prompt and the model never types them, so the labels
#' stay semantic words.
#'
#' Scoring then returns an `expected_score` column, the sum of each label's
#' value weighted by its renormalized probability. The labels of any one call
#' share a softmax, since they are read at a single position from a single
#' distribution, so this works on any instrument rather than only exclusive
#' ones.
#'
#' Nothing checks that the labels are ordered. `scores` asserts an order the
#' package cannot verify, and reversing the values reverses every score without
#' complaint.
#'
#' @param items A named list or named character vector of question strings. An
#'   unnamed single string is accepted and named `Q1`. When `exclusive = TRUE`,
#'   this may instead be a stem name (`"primary"` or `"best"`) or a custom
#'   question, with the categories supplied through `labels`.
#'
#'   A question is a free string and it is yours. It may run to a paragraph,
#'   reproduce a codebook definition verbatim, carry an exclusion clause, or
#'   include worked examples. Nothing about the scoring changes, since the
#'   question sits in the prompt and the answer is read from one token.
#' @param labels A character vector of answer words, at least two. Each element
#'   is its own label. Matching is case folded and reads by prefix, so a token
#'   counts when it opens a label and a tokenizer splitting `PRESUPUESTO` into
#'   `P` still scores. Text explaining what a label means goes in
#'   `definitions`, not here.
#'
#'   One word each, and whitespace is refused. `NOT_SURE` and `NOT-SURE` build,
#'   since joining leaves a single answer word that a tokenizer may split into a
#'   prefix of itself. `NOT SURE` does not, since one token cannot span the
#'   space and the label would be matched by `NOT` alone. Default is `c("YES",
#'   "NO")`.
#' @param ... Not used, and refused if anything reaches it. Every argument
#'   after it is name-only, so a misspelling lands here rather than being
#'   matched by position.
#' @param definitions A named character vector explaining what each label
#'   means, or `NULL` (the default). An exclusive instrument lists these in the
#'   question, so the model reads the explanation and types only the name. Any
#'   instrument can carry them, where they appear in `print()` and improve the
#'   generated probe document.
#' @param cue A string giving the instruction appended after each question.
#'   Generated from `labels` and `lang` if `NULL`. Supply your own for a
#'   language without a preset.
#' @param lang A string giving the language preset for the prompt scaffolding,
#'   `"en"` (the default) or `"es"`. Sets `cue`, `text_label`, and
#'   `answer_label` together, and any of the three can still be overridden
#'   individually. For any other language, pass all three explicitly rather than
#'   expecting a preset. See the section below for what each preset contains.
#'
#'   This is the scaffolding, not the question. A question written in Spanish
#'   under the default `"en"` preset produces a Spanish item wrapped in an
#'   English cue, which is a real prompt and rarely the intended one.
#' @param text_label,answer_label Strings giving the prompt scaffolding. Default
#'   to the `lang` preset.
#' @section Language presets:
#' Three strings the prompt is built from, plus the word used to join the labels
#' inside the cue.
#'
#' | part | en | es |
#' | :-- | :-- | :-- |
#' | `text_label` | Text | Texto |
#' | `answer_label` | Answer | Respuesta |
#' | label joiner | or | o |
#' | cue | Answer with exactly one word: ... | Responde con exactamente una palabra: ... |
#'
#' So a two-label instrument under `lang = "es"` closes with
#' \emph{Responde con exactamente una palabra: \enc{sí}{si} o no.} and
#' labels its parts \emph{Texto} and \emph{Respuesta}.
#'
#' Two stems ship for `exclusive = TRUE`, in both languages, and unlike the item
#' stems in [template_items()] neither takes a `{label}`, since the categories
#' arrive through `definitions` instead.
#'
#' | stem | en | es |
#' | :-- | :-- | :-- |
#' | `"primary"` | What is the PRIMARY subject of this text? | \enc{¿Cuál}{Cual} es el tema PRINCIPAL de este texto? |
#' | `"best"` | Which category best describes this text? | \enc{¿Qué categoría}{Que categoria} describe mejor este texto? |
#'
#' They are not interchangeable. `"primary"` asks what the text is about, which
#' assumes the categories are subjects. `"best"` asks which category describes
#' it, which assumes only that they are a scheme worth sorting into. Any other
#' string is used as the question verbatim, in whatever language it is written.
#'
#' @section Case folding and accented labels:
#' A generated token is folded to upper case before it is compared to a label,
#' so \enc{sí}{si}, \enc{Sí}{Si}, and \enc{SÍ}{SI} all count as one answer.
#'
#' [preview()] and `check_backend()` fold the same way, which is what makes the
#' preview table a reliable picture of what scoring will do.
#'
#' The first-character rule is therefore about characters and not bytes. Two
#' labels differing only in an accent still collide once folded, and this
#' function refuses them. Where a coding scheme forces that collision, `exact =
#' TRUE` is the escape.
#'
#' @param medium A string naming the stem set a preset name is looked up in, one
#'   of `"text"`, `"audio"`, `"image"` or `"video"`. Read only when `items`
#'   names a preset, since a literal question carries its own wording. There is
#'   no default, because the sets are not translations of one another. A picture
#'   mentions nothing, so `image` has `show` and `depict` where `text` has
#'   `topic` and `mention`, and `video` has both plus `discuss`, since a row
#'   holds frames and a transcript at once and the verb is what says which one
#'   the question is about.
#' @param exclusive A logical value indicating whether to build an
#'   exclusive-choice instrument. One question lists all K categories, the model
#'   picks one, and all K share a softmax at position 1. The result is a
#'   distribution, unlike an instrument of independent binary items. The
#'   category names become answer tokens here, so they must differ at their
#'   first character. Default is `FALSE`.
#' @param probe_texts A named character vector of example documents, one per
#'   label, used in place of the generated probe document, or `NULL` (the
#'   default). Supply these when the labels describe a property of the text
#'   rather than a topic, since the generated document is *about* the label
#'   rather than one that exhibits it.
#' @param exact A logical value indicating whether to score by exact match
#'   against discovered tokens rather than by prefix. Requires `backend`, since
#'   those tokens come from probing.
#'
#'   This is the one case where a label carries more than one string. Prefix
#'   matching needs the labels to differ at their first character, because a
#'   split label often emits only that character and `E` cannot be attributed
#'   between `economia` and `educacion`. Probing sends one throwaway document
#'   per label, reads back what the model actually writes, `ECON` and `EDUC`,
#'   and records it. Scoring then consults that record instead of guessing from
#'   a prefix, and a bare `E` counts for nothing and shows up in `prob_other`
#'   rather than being assigned to whichever label sorted first.
#'
#'   Nobody types those entries. Use `exact` when the labels cannot be made to
#'   differ at their first character, and rename them if they can. Default is
#'   `FALSE`.
#' @param scores A named numeric vector giving each label a value, or `NULL`
#'   (the default). Names must match the label names exactly. Supplying them
#'   asserts that the spacing between the numbers is meaningful, which is what
#'   makes an average of them defined and is a stronger claim than that the
#'   labels are ordered. Nothing checks it, since unequal spacing is legitimate
#'   on a real scale.
#' @param name A string giving a label for the instrument, carried into
#'   provenance, or `NULL` (the default).
#' @param backend An [ollama_backend()] object, or `NULL` (the default). When
#'   supplied, each label is probed against a production-shaped prompt and its
#'   observed first token is recorded. Probing checks that the labels are
#'   distinguishable at position 1 and prints what each one emits. It changes
#'   scoring only under `exact = TRUE`, since a discovered token already opens
#'   its own label and so is matched by prefix regardless.
#' @return An object of class `fathom_instrument`, a list. Its print method
#' shows the parts usually wanted, and [preview()] shows what the model will
#' actually receive.
#'
#' ## The instrument
#'
#' - `items` is the named list of questions, one per item, in the order
#'   declared. That order is the order [item_report()] and the results use, so
#'   an instrument reads as the questionnaire it was written as.
#' - `labels` is the named list of answer labels.
#' - `normalized` holds what each label is actually matched against. Under
#'   prefix matching this is the label itself, and under `exact = TRUE` it is
#'   the variants probing discovered. This is the field to read to find out why
#'   a token did or did not count.
#' - `definitions` is the category text shown to the model beneath the
#'   question, or `NULL`. Where long category wording goes, since it has no
#'   first-character constraint on it.
#' - `scores` is the numeric value per label, or `NULL`. Sits outside the prompt
#'   hash on purpose, so rescaling re-extracts nothing.
#'
#' ## How it gets asked
#'
#' - `cue` is the assembled instruction naming the permitted answers.
#' - `text_label` and `answer_label` are the words introducing the document and
#'   the answer slot.
#' - `lang` names which scaffolding preset supplied the three above.
#'
#' ## Shape and mode
#'
#' - `exclusive` says whether the labels share one distribution. `FALSE` gives
#'   independent binary items, so a document may match several.
#' - `exact_match` says whether scoring compares against probed tokens rather
#'   than by prefix. The escape for a label set that has to collide.
#' - `k` and `n_items` are the counts of labels and of items.
#' - `name` is the short label-set name used in printing.
#'
#' ## Implementation
#'
#' `probe_texts` and `firsts` are implementation. They hold what probing was run
#' against and the token each label emitted when probed, and neither is a stable
#' interface.
#' @seealso [template_items()] for items built from one stem, [preview()] for
#'   the prompt it assembles, and [measure()] for running it.
#' @examples
#' b <- instrument(
#'   items = list(
#'     econ = "Does the text discuss economic conditions?",
#'     immig = "Does the text discuss immigration?"
#'   ),
#'   labels = c("YES", "NO")
#' )
#' b
#' @export
instrument <- function(
  items,
  labels = c("YES", "NO"),
  ...,
  definitions = NULL,
  scores = NULL,
  exclusive = FALSE,
  exact = FALSE,
  probe_texts = NULL,
  cue = NULL,
  lang = c("en", "es"),
  medium = NULL,
  text_label = NULL,
  answer_label = NULL,
  name = NULL,
  backend = NULL
) {
  no_dots(..., .fn = "instrument")
  lang <- match.arg(lang)
  preset <- LANG_PRESETS[[lang]]
  text_label <- text_label %||% preset$text_label
  answer_label <- answer_label %||% preset$answer_label

  # Exact matching needs probing, but probing does not need exact matching. A
  # discovered token is always a prefix of one of its own label's variants,
  # since that is what the probe checks, so registering it changes nothing under
  # prefix matching. Coupling the two used to put an instrument into the
  # strictest mode for the label a failed probe left it knowing least about,
  # where the fallback to the literal label string scores zero on any fragment
  # that an unprobed instrument would have matched. Probing now buys the
  # distinctness check and the token map; matching stays on prefix unless asked
  # for by name.
  exact_match <- isTRUE(exact)

  # Exact matching without a probe is never the right state. Labels that survive
  # tokenization whole score the same either way, so the only reason to want
  # exact matching is that probing found something. Asking for it unprobed skips
  # the first-character check and then silently fails to match any fragment,
  # which is the failure the whole design exists to prevent.
  if (isTRUE(exact) && is.null(backend)) {
    stop(
      fmt_err(paste0(
        "`exact = TRUE` requires `backend`. Exact matching scores against what ",
        "the model emits, and that has to be discovered rather than assumed. ",
        "Drop `exact` to match by prefix instead, which needs no calls."
      )),
      call. = FALSE
    )
  }

  if (is.list(labels)) {
    stop(
      fmt_err(
        "`labels` takes one name per label, not a list.",
        paste0(
          "A list used to declare alternative spellings. Prefix matching makes ",
          "them unnecessary, since a token counts when it opens a label. Pass ",
          "a character vector. Text explaining what a label means goes in ",
          "`definitions`."
        )
      ),
      call. = FALSE
    )
  }

  # ---- exclusive choice ----------------------------------------------------
  # One question listing every category, answered with one of them. The
  # categories become answer tokens here, which is the only place in fathom
  # where tokenization constrains what you may ask.
  #
  # The listing pairs each name with its `definitions` entry. The definition
  # explains the category and the name is only what the model types, so
  # multi-word categories remain usable.
  if (isTRUE(exclusive)) {
    if (length(items) != 1L) {
      stop(
        fmt_err(paste0(
          "an exclusive instrument takes one question. Supply a stem name ",
          "(\"primary\" or \"best\") or a single question string."
        )),
        call. = FALSE
      )
    }

    # unlist() rather than a branch on type, since `items` may be a length-one
    # list here and `[[` refuses a list subscript.
    stem_key <- as.character(unlist(items, use.names = FALSE))[1]
    stem <- stem_lookup(CHOICE_STEMS, lang, medium, stem_key, "items")
    question <- stem %||% stem_key
    items <- stats::setNames(list(question), "category")

    cat_names <- if (is.null(names(labels))) {
      as.character(labels)
    } else {
      names(labels)
    }

    # Earlier versions read the listing text out of `labels`, which made that
    # argument mean answer words everywhere else and prompt text here. An
    # instrument built the old way is refused rather than silently
    # reinterpreted, since treating a definition as a label registers a token
    # the model never emits and the category then scores zero with no error.
    #
    # This fires before the general labels gate below, so an exclusive
    # instrument built the old way is told where its text belongs rather than
    # getting the generic message about names.
    if (is.null(definitions)) {
      carries_text <- !is.null(names(labels)) &&
        !identical(unname(as.character(labels)), names(labels))
      if (carries_text) {
        stop(
          fmt_err(paste0(
            "`labels` carries text that is not a label name. `labels` names ",
            "the answer words. The text shown to ",
            "the model belongs in `definitions`. Move it across, and leave ",
            "`labels` as the bare category names."
          )),
          call. = FALSE
        )
      }
      cat_desc <- cat_names
    } else {
      cat_desc <- unname(check_label_map(
        definitions,
        cat_names,
        "definitions"
      ))
    }

    listing <- paste(sprintf("%s = %s", cat_names, cat_desc), collapse = "\n")
    items[[1]] <- paste0(items[[1]], "\n\n", listing)

    if (is.null(cue)) {
      cue <- sprintf(preset$cue_fmt, paste_or(cat_names, preset$or_word))
    }

    labels <- cat_names
  }

  # ---- items ---------------------------------------------------------------
  if (is.character(items)) {
    if (is.null(names(items)) && length(items) == 1L) {
      items <- stats::setNames(as.list(items), "Q1")
    } else if (is.null(names(items))) {
      items <- stats::setNames(as.list(items), paste0("Q", seq_along(items)))
    } else {
      items <- as.list(items)
    }
  }
  if (!is.list(items) || length(items) == 0L) {
    stop(
      "`items` must be a named list or character vector of questions",
      call. = FALSE
    )
  }
  inm <- names(items)
  if (is.null(inm) || any(!nzchar(trimws(inm)))) {
    stop("every item must be named", call. = FALSE)
  }
  if (anyDuplicated(inm)) {
    stop(
      fmt_err(paste0(
        "item names must be unique. Repeated names are ",
        paste(unique(inm[duplicated(inm)]), collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }
  bad <- vapply(
    items,
    function(q) {
      !is.character(q) || length(q) != 1L || !nzchar(trimws(q))
    },
    logical(1)
  )
  if (any(bad)) {
    stop(
      fmt_err(paste0(
        "items must be single non-empty strings. Bad items are ",
        paste(inm[bad], collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }

  # ---- labels --------------------------------------------------------------
  # One string per label, and that string is the label. Declaring alternative
  # surface forms is not supported, because prefix matching already absorbs the
  # case it was meant for. A tokenizer that splits a label emits a fragment that
  # opens it, and a fragment is matched. Measured across ten Ollama models and
  # six tokenizer families, on labels from two to eleven characters, no model
  # ever emitted a position-1 token that was not a fragment of a label.
  #
  # `normalized` is still a list, because it is the matching surface rather than
  # a copy of this argument, and probe_labels() writes the discovered token into
  # it under `exact = TRUE`. That is the one place a label carries more than one
  # string, nobody types it, and it is what lets labels sharing a first
  # character be told apart.
  if (!is.character(labels)) {
    stop("`labels` must be a character vector", call. = FALSE)
  }
  if (!is.null(names(labels)) && !identical(unname(labels), names(labels))) {
    stop(
      fmt_err(
        "`labels` is named, and the names do not match the labels.",
        paste0(
          "Naming used to declare an alternative spelling for each label. ",
          "Prefix matching makes that unnecessary. Drop the names, or move ",
          "explanatory text into `definitions`."
        )
      ),
      call. = FALSE
    )
  }
  labels <- stats::setNames(as.list(unname(labels)), unname(labels))
  if (length(labels) < 2L) {
    stop("`labels` must give at least two labels", call. = FALSE)
  }
  lnm <- names(labels)
  if (is.null(lnm) || any(!nzchar(trimws(lnm)))) {
    stop("every label must be named", call. = FALSE)
  }
  if (anyDuplicated(lnm)) {
    stop(
      fmt_err(paste0(
        "label names must be unique. Repeated names are ",
        paste(unique(lnm[duplicated(lnm)]), collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }
  lbad <- vapply(
    labels,
    function(v) {
      !is.character(v) || length(v) == 0L || any(!nzchar(trimws(v)))
    },
    logical(1)
  )
  if (any(lbad)) {
    stop(
      fmt_err(paste0(
        "labels must be non-empty strings. Bad labels are ",
        paste(lnm[lbad], collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }

  # A label is one whitespace-free answer word, and until now that was
  # documented rather than enforced.
  #
  # Nothing about a spaced label fails loudly. Under prefix matching NOT opens
  # NOT SURE, and under exact matching the probe registers whatever the model
  # emitted first, which is also NOT. Either way the label scores P(NOT),
  # summed over NOTHING and every other continuation, and the slack is
  # unbounded rather than the narrow slack a split single word leaves. No
  # column reports the difference.
  #
  # The cue is the second half of it. It promises exactly one word in both
  # language presets, so naming a two-word option contradicts the instruction
  # in the same sentence, and whatever that costs in compliance lands in
  # prob_other attributed to the question.
  #
  # The test is whitespace rather than word count, which is what makes it
  # right for a hyphenated label, an underscored one, and a script that does
  # not space its words. All three are single answer words that a tokenizer may
  # split, and a fragment of one is still a prefix of the whole label.
  #
  # Through stringi rather than a base character class, for the reason the
  # folding below goes through it. Base `[[:space:]]` misses U+00A0 under both
  # regex engines, so a non-breaking space would pass here and fail at scoring.
  #
  # Leading and trailing whitespace is refused rather than trimmed, since
  # trimws() runs on the matching surface and not on the label name. A trimmed
  # label would match as one string and print into the cue as another.
  spaced <- stringi::stri_detect_regex(lnm, "[[:space:]]")
  if (any(spaced)) {
    stop(
      fmt_err(
        paste0(
          "labels must not contain whitespace. Offending label(s) are ",
          paste(sprintf("\"%s\"", lnm[spaced]), collapse = ", "),
          "."
        ),
        paste0(
          "Scoring reads one token at generated position 1 and the cue asks ",
          "for exactly one word, so a label spanning a space is matched by ",
          "its opening word alone and everything after it is never read. That ",
          "does not score zero, it scores the opening word, and no column ",
          "separates the two."
        ),
        paste0(
          "Join them if the answer word really is compound, since NOT_SURE ",
          "and NOT-SURE are both accepted. Otherwise move the wording to ",
          "where it has no length limit, which is the question for an ",
          "instrument of items and `definitions` for an exclusive one."
        )
      ),
      call. = FALSE
    )
  }

  normalized <- lapply(labels, function(v) {
    unique(stringi::stri_trans_toupper(trimws(v)))
  })
  flat <- unlist(normalized, use.names = FALSE)
  if (anyDuplicated(flat)) {
    stop(
      fmt_err(paste0(
        "two labels normalize onto the same string. Matching is case folded, ",
        "so labels differing only in case collide. Repeated ",
        "variants are ",
        paste(unique(flat[duplicated(flat)]), collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }

  # ---- first characters ----------------------------------------------------
  # Scoring counts a token as a label when it opens one of that label's
  # variants, so two labels sharing a first character cannot be told apart. A
  # single-character token is not hypothetical; across six tokenizer families it
  # was the commonest thing a split label emitted.
  #
  # Requiring distinct first characters means no two labels share any prefix at
  # all, so every match resolves to one label at any token length. Probing is
  # the way out when the labels have to collide.
  # Over every declared variant, not just the label names, since a variant is
  # something the model might write and scoring treats it as evidence.
  firsts_by <- lapply(normalized, function(v) unique(substr(v, 1, 1)))
  all_firsts <- unlist(firsts_by, use.names = FALSE)
  if (!exact_match && anyDuplicated(all_firsts)) {
    dup <- unique(all_firsts[duplicated(all_firsts)])
    clash <- vapply(
      dup,
      function(ch) {
        paste(
          lnm[vapply(firsts_by, function(f) ch %in% f, logical(1))],
          collapse = " and "
        )
      },
      character(1)
    )
    stop(
      fmt_err(
        paste0(
          "labels must differ at their first character. ",
          paste(clash, collapse = "; "),
          "."
        ),
        paste0(
          "Scoring reads one token, and a split label often emits only its ",
          "first character, which cannot then be attributed. Rename so the ",
          "labels differ earlier, or pass `exact = TRUE` with a backend to ",
          "discover what each label emits instead."
        )
      ),
      call. = FALSE
    )
  }

  # ---- definitions and probe texts ----------------------------------------
  # Both are name-to-text maps keyed on the label names. `definitions` was
  # already validated inside the exclusive branch, against the same names, so
  # this is idempotent there and is the only check on a non-exclusive
  # instrument.
  if (!is.null(definitions)) {
    definitions <- check_label_map(definitions, lnm, "definitions")
  }
  if (!is.null(probe_texts)) {
    probe_texts <- check_label_map(probe_texts, lnm, "probe_texts")
  }

  # ---- scores --------------------------------------------------------------
  if (!is.null(scores)) {
    if (!is.numeric(scores) || is.null(names(scores))) {
      stop(fmt_err("`scores` must be a named numeric vector"), call. = FALSE)
    }
    if (anyNA(scores)) {
      stop(fmt_err("`scores` must not contain NA"), call. = FALSE)
    }
    if (!setequal(names(scores), lnm)) {
      stop(
        fmt_err(paste0(
          "`scores` must name every label exactly once. Labels are ",
          paste(lnm, collapse = ", "),
          ". Scores name ",
          paste(names(scores), collapse = ", "),
          "."
        )),
        call. = FALSE
      )
    }
    # Stored in label order so score_labels() multiplies element-wise against a
    # probability vector without indexing by name on every call.
    scores <- scores[lnm]
  }

  # ---- cue -----------------------------------------------------------------
  if (is.null(cue)) {
    # Through paste_or() rather than a plain collapse, so a three-label
    # non-exclusive instrument reads "a, b, or c" like the exclusive branch does
    # instead of "a or b or c".
    cue <- sprintf(preset$cue_fmt, paste_or(lnm, preset$or_word))
  }
  if (!is.character(cue) || length(cue) != 1L || !nzchar(trimws(cue))) {
    stop("`cue` must be a single non-empty string", call. = FALSE)
  }

  out <- structure(
    list(
      items = items,
      labels = labels,
      normalized = normalized,
      cue = cue,
      text_label = text_label,
      answer_label = answer_label,
      lang = lang,
      exclusive = isTRUE(exclusive),
      exact_match = exact_match,
      definitions = definitions,
      scores = scores,
      probe_texts = probe_texts,
      firsts = NULL,
      k = length(labels),
      n_items = length(items),
      name = name %||% paste(lnm, collapse = "/")
    ),
    class = "fathom_instrument"
  )

  # Probing verifies the labels are distinguishable at position 1 and records
  # what token each label emits, which is registered as a scoring variant. One
  # call per label, and the label has to win its own call. The emitted token
  # depends on the surrounding prompt, so it cannot be derived from the label
  # string.
  if (!is.null(backend)) {
    out <- probe_labels(out, backend)
  }
  out
}

#' Validate a named vector against a label set
#'
#' `definitions` and `probe_texts` are both name-to-text maps keyed on the
#' label names, so they are checked the same way and returned in label order.
#'
#' @param x A named vector, as supplied.
#' @param nms A character vector of label names, in order.
#' @param arg A string giving the argument name, for the message.
#' @return `x`, reordered to match `nms`.
#' @keywords internal
#' @noRd
check_label_map <- function(x, nms, arg) {
  if (
    !is.character(x) ||
      is.null(names(x)) ||
      anyNA(x) ||
      any(!nzchar(trimws(x)))
  ) {
    stop(
      fmt_err(paste0(
        "`",
        arg,
        "` must be a named character vector with no blanks"
      )),
      call. = FALSE
    )
  }
  if (!setequal(names(x), nms)) {
    stop(
      fmt_err(paste0(
        "`",
        arg,
        "` must name every label exactly once. Labels are ",
        paste(nms, collapse = ", "),
        ". `",
        arg,
        "` names ",
        paste(names(x), collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }
  x[nms]
}

#' Join names with a trailing conjunction
#'
#' Two items take no comma, since "A, or B" is not English. Three or more take
#' the serial comma.
#'
#' @param x A character vector of names.
#' @param or_word A string giving the conjunction.
#' @return A string joining `x` with commas and `or_word` before the last, or
#'   `x` itself when it has fewer than two elements.
#' @keywords internal
#' @noRd
paste_or <- function(x, or_word = "or") {
  n <- length(x)
  if (n < 2L) {
    return(paste(x))
  }
  if (n == 2L) {
    return(paste(x[1], or_word, x[2]))
  }
  paste0(
    paste(utils::head(x, -1), collapse = ", "),
    ", ",
    or_word,
    " ",
    utils::tail(x, 1)
  )
}

#' Discover each label's first token against a live model
#'
#' @description
#' Runs one call per label against a production-shaped prompt, reads back the
#' token the model emits first, and registers it as a scoring variant. Returns
#' the instrument with `firsts` attached.
#'
#' @section Why the model has to emit the label:
#' The question is not whether the label exists as a token in the model's
#' vocabulary. It is which token the model emits when it means that label, and
#' those are different.
#'
#' A cheaper design was tried and reverted. It sent one call, read the whole
#' returned distribution, and treated a label appearing among the returned
#' tokens as proof that it was a single token. On gemma4 the token `BUDGET`
#' appears in the distribution and the model still emits `BU` when it answers
#' BUDGET, because both are real tokens and the argmax path runs through the
#' shorter one. Registering `BUDGET` on that evidence made the category score
#' zero on every document, with no error raised.
#'
#' So each label gets its own call, and the label has to win it.
#'
#' @section Discovery rather than assumption:
#' Scoring reads generated position 1 and matches that one token against the
#' label. Multi-syllable labels rarely survive tokenization, and which ones do
#' cannot be predicted from the strings.
#'
#' A label's first token also depends on the surrounding prompt, so the probe
#' prompt has the same shape as the prompts that will be scored. This constructs
#' a document about each label rather than asking the model to repeat a string.
#'
#' @section What it checks:
#' Distinctness is the requirement. Two labels emitting the same first token are
#' indistinguishable at position 1, and that is fatal. Splitting is not fatal,
#' since the observed token becomes a variant.
#'
#' A split always emits a prefix of the label it came from. A token that is not
#' a prefix means the probe did not force that label at all, which is checked
#' separately and reported separately, since the remedy is a different one.
#'
#' @section When the generated document does not work:
#' It reads "This text is entirely and exclusively about X", where X is the
#' label's `definitions` entry if it has one and the bare label name otherwise.
#'
#' A definition usually makes that a usable document. A bare name often does
#' not. "Entirely and exclusively about municipal budget, spending, or revenue"
#' is a document; "entirely and exclusively about passing" is not, and every
#' label then gets the same answer. Giving the labels definitions is the first
#' thing to try.
#'
#' What definitions cannot fix is a label naming a property the sentence is
#' merely about rather than exhibiting, or an instrument whose first item asks
#' about an aspect the generated document does not mention. `probe_texts`
#' replaces the generated document with one you supply, and is the remedy for
#' both.
#'
#' @section The approximation a split introduces:
#' The probability of a first token sums over every continuation, not only the
#' label it was discovered for, so it is an upper bound on the label's own
#' probability. The slack is real and not measurable from a single call.
#'
#' @param instrument A `fathom_instrument`.
#' @param backend An [ollama_backend()] object.
#' @return The instrument, with discovered tokens registered in `normalized` and
#'   recorded in `firsts`.
#' @keywords internal
#' @noRd
probe_labels <- function(instrument, backend) {
  nms <- names(instrument$labels)

  firsts <- vapply(
    nms,
    function(lab) {
      # A definition makes a better generated document than a bare label name,
      # since "entirely and exclusively about municipal budget, spending, or
      # revenue" is a document and "entirely and exclusively about budget" is
      # barely one.
      doc <- if (!is.null(instrument$probe_texts)) {
        instrument$probe_texts[[lab]]
      } else {
        paste0(
          "This text is entirely and exclusively about ",
          (instrument$definitions %||% instrument$descriptions)[[lab]] %||%
            instrument$labels[[lab]][1],
          "."
        )
      }
      p <- build_prompt(doc, instrument$items[[1]], instrument)
      r <- tryCatch(ollama_generate(p, backend), error = function(e) e)
      if (inherits(r, "error")) {
        return(NA_character_)
      }
      lp <- tryCatch(r$logprobs[[1]]$top_logprobs, error = function(e) NULL)
      if (is.null(lp) || length(lp) == 0L) {
        return(NA_character_)
      }
      # A blank token is not a discovery. startsWith(label, "") is TRUE, so it
      # would pass the forced check, register "" as a variant, and then match
      # every whitespace token under exact scoring.
      tok <- trimws(lp[[1]]$token %||% "")
      if (!nzchar(tok)) NA_character_ else tok
    },
    character(1)
  )

  if (anyNA(firsts)) {
    warning(
      fmt_warn(paste0(
        "could not probe label(s) ",
        paste(nms[is.na(firsts)], collapse = ", "),
        ". Scoring falls back to the label strings as written. Under prefix ",
        "matching a token counts when it opens a label, so the fallback still ",
        "matches what a split label emits. Under `exact = TRUE` it does not, ",
        "and a label the tokenizer splits scores zero."
      )),
      call. = FALSE
    )
    return(instrument)
  }

  norm_firsts <- stringi::stri_trans_toupper(firsts)
  up <- stringi::stri_trans_toupper(nms)

  # A tokenizer split always emits a prefix of the label it came from, which is
  # how BUDGET gives BU. A token that is not a prefix means the probe document
  # did not force that label and the model answered something else, which is a
  # different failure with different advice. Against every declared variant
  # rather than the label name. An instrument that declares an accented form
  # means the model may write it, and emitting one is a correct answer rather
  # than a probe that failed to separate the labels.
  forced <- vapply(
    seq_along(nms),
    function(i) any(startsWith(instrument$normalized[[i]], norm_firsts[i])),
    logical(1)
  )

  if (!all(forced)) {
    # Which label emitted what, since "not a prefix" covers a model that read
    # the probe and chose a different label as well as a model that emitted
    # something structural. The remedy differs and the tokens tell them apart.
    emitted <- paste(
      sprintf("%s emitted %s", nms[!forced], firsts[!forced]),
      collapse = ", "
    )
    # Three situations, three remedies. Which one applies depends on what the
    # probe had to build its document from, so the message says what it used
    # rather than describing a document that was not sent.
    advice <- if (!is.null(instrument$probe_texts)) {
      paste0(
        "`probe_texts` was supplied, so the model read your example and chose ",
        "a different label for it. Either the example does not sit clearly ",
        "enough in its own category, or the labels are too close together to ",
        "separate on one document each. The emitted labels above say which ",
        "category the model thought each example was."
      )
    } else if (is.null(instrument$definitions %||% instrument$descriptions)) {
      paste0(
        "These labels have no `definitions`, so the probe had only the label ",
        "name to build from and sent \"This text is entirely and exclusively ",
        "about ",
        nms[!forced][1],
        "\", which is not a document about ",
        "anything. Give the labels definitions and the generated probe ",
        "usually separates them. Supply `probe_texts` only if it still does ",
        "not."
      )
    } else {
      paste0(
        "The probe built its document from each label's definition, and that ",
        "was not enough to place it in its own category. Two things do this. ",
        "A label naming something a sentence can be about without exhibiting ",
        "it, and an instrument whose first question asks about an aspect the ",
        "generated document never mentions. Supply `probe_texts` with one ",
        "short example document per label, written to exhibit its label ",
        "rather than describe it."
      )
    }
    stop(
      fmt_err(paste0(
        "the probe did not separate the labels. ",
        emitted,
        ". None of those tokens is a prefix of its own label, so the probe ",
        "document did not read as an instance of the label it was written for. ",
        advice
      )),
      call. = FALSE
    )
  }

  if (anyDuplicated(norm_firsts)) {
    dup <- unique(norm_firsts[duplicated(norm_firsts)])
    stop(
      fmt_err(
        paste0(
          "labels emit the same first token (",
          paste(dup, collapse = ", "),
          "). Affected labels are ",
          paste(nms[norm_firsts %in% dup], collapse = ", "),
          "."
        ),
        paste0(
          "Position-1 scoring cannot separate them. Rename the labels so they ",
          "differ earlier, or use independent binary items via ",
          "template_items(), where the category sits in the question and never ",
          "needs to be an answer token."
        )
      ),
      call. = FALSE
    )
  }

  # Register the observed token as a variant. score_labels() reads
  # instrument$normalized, so nothing downstream changes.
  for (i in seq_along(nms)) {
    instrument$normalized[[nms[i]]] <-
      unique(c(instrument$normalized[[nms[i]]], norm_firsts[i]))
  }
  instrument$firsts <- stats::setNames(firsts, nms)

  split <- norm_firsts != up
  if (any(split)) {
    message(fmt_note(
      "Note",
      "Labels split by the tokenizer. Scoring on first tokens.",
      paste(sprintf("%s -> %s", nms[split], firsts[split]), collapse = ", "),
      paste0(
        "P(first token) is an upper bound on P(label). Check prob_other in ",
        "item_report(). If it stays small, the slack is too."
      )
    ))
  }

  instrument
}

#' Build items by substituting categories into one question stem
#'
#' @description Turns a list of categories into an instrument of independent
#'   binary items. The category goes into the question, not the answer, so
#'   categories may be any length and the tokenization constraint never applies.
#'
#' @section The named stems:
#' Four sets, one per medium, each in English and in Spanish. `medium` picks
#' the set and `lang` picks the language.
#'
#' | medium | stems |
#' | :-- | :-- |
#' | `"text"` | topic, mention, express, about, frame |
#' | `"audio"` | topic, mention, express, about, frame |
#' | `"image"` | show, depict, express, about, frame |
#' | `"video"` | show, depict, discuss, express, about, frame |
#'
#' The sets are not translations of one another, which is why `medium` has no
#' default.
#'
#' The `text` set in full. The other three take the same shape with their own
#' noun and their own verbs.
#'
#' | stem | en | es |
#' | :-- | :-- | :-- |
#' | `"topic"` | Does this text discuss \{label\}? | \enc{¿}{}Este texto trata sobre \{label\}? |
#' | `"mention"` | Does this text mention \{label\}? | \enc{¿}{}Este texto menciona \{label\}? |
#' | `"express"` | Does this text express \{label\}? | \enc{¿}{}Este texto expresa \{label\}? |
#' | `"about"` | Is this text primarily about \{label\}? | \enc{¿}{}Este texto trata principalmente de \{label\}? |
#' | `"frame"` | Does this text frame the issue in terms of \{label\}? | \enc{¿}{}Este texto enmarca el tema en \enc{términos}{terminos} de \{label\}? |
#'
#' These differ in what they assert, not in topic. `"mention"` fires on a
#' passing reference. `"about"` requires the text to be centrally concerned with
#' the category. The choice is a measurement decision, and it is the same
#' decision in either language, since the two halves of a row are translations
#' of one another rather than separate instruments.
#'
#' A custom string ignores `lang` entirely, so a Spanish question written by
#' hand is passed through as typed. The scaffolding around it is a separate
#' setting, on [instrument()], and the two can disagree. See its `lang`
#' argument.
#'
#' @param stem A named stem, or a custom string containing `{label}`.
#' @param medium A string naming the stem set a named stem is looked up in, one
#'   of `"text"`, `"audio"`, `"image"` or `"video"`. Read only for a named stem,
#'   since a custom string carries its own wording. There is no default, because
#'   the sets differ by medium rather than translating, so a default would
#'   answer a question about the document with a guess.
#' @param categories A character vector of categories. Names, if present, become
#'   item names.
#' @param lang A string giving the language for the named stems, `"en"` (the
#'   default) or `"es"`. Ignored for a custom string, which is used exactly as
#'   written. This sets the question only. The scaffolding around it is
#'   [instrument()]'s own `lang`, and leaving that at its default gives a
#'   Spanish question under an English cue.
#' @return A named list of questions, one per category, ready to pass to
#'   [instrument()] as its first argument.
#'
#'   Names are the names of `categories`, so they become the item names in the
#'   results and in [item_report()]. Pass a named vector to control them. An
#'   unnamed vector uses the values themselves, which rarely suits a category
#'   that runs to a phrase.
#'
#'   Values are the stem with `{label}` replaced by each category's text. Order
#'   is preserved, and an instrument reads in the order its items were declared
#'   rather than alphabetically.
#'
#'   The result is an ordinary list, so an item can be edited before it reaches
#'   [instrument()] when one category needs wording the stem cannot give it.
#' @seealso [instrument()], which takes the items.
#' @examples
#' template_items("topic", c(econ = "the economy", immig = "immigration"),
#'                medium = "text")
#'
#' # The same stem name asks a different question of a different document.
#' template_items("about", c(econ = "the economy"), medium = "image")
#'
#' # A custom string carries its own wording, so it needs no medium.
#' template_items("Does the author appear to endorse {label}?", c("austerity"))
#' @export
template_items <- function(
  stem,
  categories,
  lang = c("en", "es"),
  medium = NULL
) {
  lang <- match.arg(lang)
  if (!is.character(stem) || length(stem) != 1L) {
    stop("`stem` must be a single string", call. = FALSE)
  }

  preset <- stem_lookup(ITEM_STEMS, lang, medium, stem, "stem")
  if (!is.null(preset)) {
    stem <- preset
  }

  if (!grepl("{label}", stem, fixed = TRUE)) {
    stop(
      fmt_err(
        paste0(
          "`stem` must contain {label}, or name a preset for a medium named ",
          "in `medium`."
        ),
        paste0(
          "Presets by medium: ",
          paste(
            vapply(
              STEM_MEDIA,
              function(m) {
                paste0(
                  m,
                  " (",
                  paste(names(ITEM_STEMS[[lang]][[m]]), collapse = ", "),
                  ")"
                )
              },
              character(1)
            ),
            collapse = "; "
          ),
          "."
        )
      ),
      call. = FALSE
    )
  }
  nm <- names(categories) %||% categories
  stats::setNames(
    as.list(vapply(
      categories,
      function(x) gsub("{label}", x, stem, fixed = TRUE),
      character(1)
    )),
    nm
  )
}

#' @export
print.fathom_instrument <- function(x, ...) {
  labs <- names(x$labels)
  # Every label maps to its own name, so a per-label block would be K identity
  # lines. Anything the probe discovered is shown on the Probed line.
  # An instrument built before the rename keeps its definitions under the
  # old field name, which is still read.
  defs <- x$definitions %||% x$descriptions
  descs <- if (!is.null(defs)) {
    dw <- max(nchar(names(defs)))
    sprintf(
      "%-*s  %s",
      dw,
      names(defs),
      unlist(defs, use.names = FALSE)
    )
  }
  split <- FALSE
  probed <- NULL
  if (!is.null(x$firsts)) {
    split <- any(
      stringi::stri_trans_toupper(x$firsts) !=
        stringi::stri_trans_toupper(names(x$firsts))
    )
    probed <- paste(
      sprintf("%s -> %s", names(x$firsts), x$firsts),
      collapse = ", "
    )
  }
  cat(
    "Instrument\n\n",
    fmt_fields(list(
      # The default name is the label set, which the Labels line already shows.
      Name = if (!identical(x$name, paste(labs, collapse = "/"))) x$name,
      Shape = if (isTRUE(x$exclusive)) {
        "exclusive choice, so the labels share one softmax"
      } else {
        "independent items"
      },
      Items = sprintf(
        "%d (%s)",
        x$n_items,
        fmt_names(names(x$items), max = 8L)
      ),
      Labels = sprintf("%d (%s)", x$k, fmt_names(labs, max = 8L)),
      # Directly under the labels, since that is what they explain.
      Definitions = descs,
      Scores = if (!is.null(x$scores)) {
        paste(sprintf("%s %g", names(x$scores), x$scores), collapse = ", ")
      },
      Cue = gsub("\n", "\\\\n", x$cue),
      Probed = probed,
      Matching = if (isTRUE(x$exact_match)) {
        "exact, so a token counts only when it is a label"
      } else {
        "by prefix, so a token counts when it opens a label"
      }
    )),
    "\n",
    sep = ""
  )
  if (split) {
    cat_note(
      "Note",
      "Scoring is on first tokens, so P(token) is an upper bound."
    )
  }
  invisible(x)
}
