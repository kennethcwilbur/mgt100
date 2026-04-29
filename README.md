# MGT 100 Slide Decks

## Change Log

### 2026-04-22, ~1:40-2:15 PM PT

Task: Phase 2 of mgt100-07 (Branding) revision — produced a full clean-revealjs rewrite at `mgt100-07-v2.qmd` with all Yavorsky additions (G1-G7), a section-structured Brand Advertising block, an intermission at 75%, and a Nash-equilibrium Competition slide. Original `mgt100-07.qmd` preserved untouched pending user review.

Inputs reviewed:
- `mgt100-07.qmd` (legacy-format source, 53 `##` content slides + 3 `#` section headers)
- `mgt100_07_script.R` (celebrity-endorsement profit analysis, 2-D grid search on S1/S2)
- Yavorsky `Class 8 - Branding.pdf` (77 pages, skimmed via subagent for novel content)
- CLAUDE.md protocol, MEMORY.md, prior README entries

Structure — 75 slides on 75 PDF pages, 1 title + 8 section headers + 66 content slides:

- `# Branding` — outline, logos, G1 perception primer (3 slides: McGurk, Dress, Thatcher), Gap/UC/Westinghouse logo fails, Netflix case (7), Kraft, What is a brand? (cow + memory + definition), G3 Swaminathan brand categories, auto-associative sequences, Economist lightbulb, Brand Safety, G2 prefix-priming game
- `# How do Brands Work?` — Credibility (Bayer, Bronnenberg 2015 + 2020), Rapid communication (Gogurt), Personality (Geico + tattoos + Brands Can), Gestalt (peanut butter + search), Artificial differentiation (vodka + G6 bottled water), What's in a name? (toothfish), G7 Keller Brand Report Card, G4 House of Brands vs. Branded House (2 slides)
- `# Brand Advertising` — section umbrella with 4 subheads (`# Brand vs. Performance advertising`, `# Programmatic advertising`, `# Does advertising build brands?`, `# Measuring ad impact`)
- Brand vs. Performance: brandformance + Brand is Dead?
- Programmatic: programmatic definition + prog_pros + Zuckerberg-on-AI-ads + Programmatic downsides + LUMAscape
- Intermission (slide 56 = 75% of 75)
- Does advertising build brands: Airbnb ×3 + DJW19 Toyota/Coke/Apple + Advertising & brand attitudes (Hartmann-Lemmon-Wilbur 2019 prisoner's dilemma)
- Measuring ad impact: Ads Measurement Approaches, Tying Things Together, Toolkit (WTP for sponsorship), G5 Rebranding Successes + G5 Rebranding Fails
- `# Wrapping up` — Class script, Competition (Nash-equilibrium with celebrity-endorsement scenarios A & B), Recap, Going further

Yavorsky additions (11 new content slides):
- G1 Perception primer (3 slides) — `yav_perception_mcgurk.png`, `yav_perception_dress.png`, `yav_perception_thatcher.png` (extracted from Yavorsky pp. 6, 8, 9, trimmed with 20px white border)
- G2 Prefix priming (1 slide) — `yav_prefix_priming.png` (p. 29)
- G3 Swaminathan brand categories (1 slide) — `yav_swaminathan_brands.png` (p. 30)
- G4 House of Brands vs. Branded House (2 slides) — `yav_hob_vs_bh.png` + `yav_grocery_store.png` (pp. 62-63)
- G5 Rebranding successes + fails (2 slides) — `yav_rebrand_success.png` + `yav_rebrand_fail.png` (pp. 67, 70)
- G6 Bottled water 4,400× markup (1 slide) — `yav_bottled_water.png` (p. 44)
- G7 Keller Brand Report Card (1 slide) — `yav_keller_report_card.png` (p. 47)

Nash-equilibrium Competition slide (per user spec):
- Scenario A: Samsung hires celebrity. For each A1 price on a grid, find S1 profit-maximizing price; for each S1 price on a grid, find A1 profit-maximizing price (A1 mc = \$450); intersect best-response curves to find (p_S1, p_A1) Nash equilibrium
- Scenario B: repeat with Apple hiring the celebrity (CCE on A1/A2 utilities)
- Plot all 4 best-response curves in (p_A1, p_S1) space, mark both Nash equilibria, compare profits
- Callout flags: "most challenging competition of the quarter. It's absolutely OK if you can't fully figure it out -- the goal is to stretch, not to score"

Template reformat applied (CLAUDE.md Steps 2-11):
- YAML: bolded title, dual-author block with HTML anchors, `date: today`, dynamic date-format, chalkboard, custom.css, gtag.html
- 12 `::: aside` blocks converted — sources to `::: {.source-url}`, commentary to `::: {.callout-note}`
- All 46 image references kept; sizing standardized from `width="Xin"` to `height="Npx"`
- 2 `.scrollable` instances removed (replaced with `.smaller` where needed)
- Over-indented code-block sub-bullets converted to proper 4-space nested lists (~15 instances)
- `target="_blank"` audited and added where missing
- Canonical slide ordering enforced (title → image → callout → source-url → notes)
- Pipe separators reserved for `::: {.source-url}`; callouts use flowing prose
- Malformed markdown fixed: `{fig-align="center width=10in}` on vodka line → `{fig-align="center" height="450px"}`
- Standard closing images: `scroll.jpg` / `recap.png` / `takingoff.png` / `competition.png` absolute-positioned bottom-right, `intermission.jpg` centered 400px

66 SUGGESTION-prefixed callout notes written for every `##` content slide, drawing from existing speaker notes where substantive. Section-header `#` slides intentionally have no callouts.

Render and verification:
- `quarto render mgt100-07-v2.qmd` → HTML clean
- Local HTTP server (port 8765) + `chromium --headless --print-to-pdf --virtual-time-budget=180000 --run-all-compositor-stages-before-draw` → PDF 20.6 MB
- First pass: 78 PDF pages for 75 slides (3 overflows detected)
- Overflows identified via `pdftoppm -r 80` screenshots of every page + Read on each PNG to match corner counter to page number:
    - slide 34 (Rapid communication / Gogurt) — image + long callout
    - slide 35 (Personality / Geico two-column) — 3 images + long callout
    - slide 44 (House of Brands vs. Branded House) — image + callout + source-url
- Fixes: reduced gogurt 280px→210px and shortened its callout; reduced Geico column images 220px→180px / 460px→380px and shortened callout; reduced HoB image 430px→370px and shortened callout
- Re-rendered → 75 PDF pages on 75 slides, confirmed via full screenshot sweep
- Spot-checked: title slide, both section-header subheads, Perception McGurk, auto-associative, Brand Safety, Prefix priming, PL Share, Programmatic, Zuck quote, Intermission (page 59, confirms 75% placement for slide 56), Toyota attitudes, Toolkit (math renders cleanly), Keller, HoB/BH, grocery store, Rebranding successes + fails, Competition (Nash eq content + "most challenging competition of the quarter" callout present), Recap, Going further (corner counter = 75)

Yavorsky image prep:
- `pdftoppm -f N -l N -r 150 -png "Class 8 - Branding.pdf" /tmp/yav_...`
- `convert FILE -trim -fuzz 5% -bordercolor white -border 20 +repage FILE` — trimmed all 11 extracts to ~1600×900 (16:9 after border)
- 11 new files in `images/` prefixed `yav_` for easy identification

Files changed:
- `mgt100-07-v2.qmd` (new) — 47 KB
- `mgt100-07-v2.html` (new) — 108 KB
- `mgt100-07-v2.pdf` (new) — 20.6 MB
- `images/yav_*.png` (11 new) — total ~4 MB

Verification outcome:
- HTML renders without errors (only Quarto config echo in log)
- PDF 75 pages = 75 slides; corner counter matches page number end-to-end
- All `SUGGESTION:` callouts present; all 11 Yavorsky images display; Nash-equilibrium competition slide content + "most challenging" callout present; intermission at expected position

Not yet performed:
- User review / Phase 3 interactive line-by-line revision
- Rename `mgt100-07-v2.*` → `mgt100-07.*` (deferred until user approves)
- GitHub push (deferred)

### 2026-04-21, afternoon PT

Task: Continuation of mgt100-06.qmd interactive revision. User addressed remaining SUGGESTION-prefixed callouts from slide 25 onward and restructured the Decoy/Pricing-elasticity portion of the deck. HTML re-rendered and PDF re-rendered + screenshotted after each edit.

Slide-level changes (corner-counter numbering as of end of session):

- **Slide 9 (Pricing strategies are secret):** callout → "What are the costs and benefits of price strategy transparency?"
- **Slide 13 (Value Pricing):** callout rewritten to start-up framing "Some of you will start companies, and you will use EVC..."
- **Slide 16 (Batteroo Boost):** speaker-notes only — updated 3 occurrences of "Batteriser" → "Batteroo Boost" for consistency with slide body
- **Slide 18 (Why not just ask WTP):** "operating or pricing decision" → "product launch decision or pricing decision"; callout comma → semicolon; "unincentivized" → "non-incentivized"
- **Slide 19 (Van Westendorp):** goal bullet "Estimate stated WTP range" → "Elicit distributions of customer price perceptions"; "Acceptable Price Range" wrapped in quotes
- **Slide 20 (VW output):** added `[Van Westendorp (1976)]` source-url block
- **Slide 22 (Signals/Perceived Quality):** "will you get 100% market share?" → "will you be able to corner the market?"
- **Slide 23 (Price as a signal / Nadal):** "Do you think" → "How much do you think"
- **Slide 24 (Krishna figure):** callout extended with ". MGT 104 & 107 go deeper"
- **Slide 25 (Cognitive Costs):** callout extended with "Counterpoint: A limited amount of friction can help sellers of complex products to screen customers, engage customers, and communicate with customers"
- **Slide 28 (Left-digit: Lyft):** callout extended with "(But why did they draw their demand curve upside down?!)"
- **Old slide 29 ↔ 30 swap:** Price salience (now slide 29) became first; Anchoring (now slide 30) became second
- **Slide 29 (Price salience):** restructured to two-column layout with bullets + new callout (quoting "If you have to ask, you can't afford it.") in left column, carbone_menu.png image (enlarged to 423px) in right column
- **Slide 30 (Anchoring):** "boba" → "fancy boba", "Camellia Rd Tea Bar" → "The tea shop", "$13" → "$11"; Yeti prices "$110 → $118" and "$95 → $110"; "La Jolla Village Drive" → "Balboa" (4x); rewrote callout to "Where do people get their reference prices? And when does bargain hunting make us happy?"; wrapped long beach paragraph in 0.75em div; enlarged "Your turn" bullets to 1.3em; initially enlarged then reverted callout/source font size to regular; fixed Thaler (2008) previously broken link with Google Drive URL
- **Slides 31-32 (Decoy Effects, combined):** merged old slides 31 + 32 into a single two-column slide titled "Decoy Effects"; bullets promoted from sub-bullets; huberputo.png image placed between bullets and callout; 4-line callout with Proportionality (MNL: IIA property) / Substitutability (Heterogeneous Logit) / Attraction (decoy) / "All three effects may operate simultaneously to explain customer purchase data"; Huber & Puto (1983) source link added
- **New slide 32 (Decoy examples image):** new image-only slide using decoy-examples.png (500px) with callout "Decoy pricing is common in some industries, especially Software-as-a-Service (SaaS). Sometimes price menus become cluttered"
- **New slide 33 (When do Attraction/Decoy Effects Obtain?):** brand-new slide with 4 bullets summarizing decoy replication literature (Yang & Lynn 2014, Frederick et al. 2014, Bogard et al. 2024, + Wu & Cosguner / Devine / Rafai field evidence); callout on "decoy pricing overextended beyond clear dominance/dominated relationships; be careful outside vertical product differentiation, never implement without testing"; 6 source links; fixed typos "coudl → could" and "mroe → more"
- **Deleted old slides 34-39:** full diamonds deep-dive (Field evidence, 4 vertical attributes, Estimating Decoy Effects, Dominant-Dominated Pair, Model estimates, counterpoint) — replaced conceptually by new slide 33 summary
- **Slide 34 (Price discrimination, formerly 40):** removed `.smaller`; shortened "Reduce resentment via new/loyal..." → "New/loyal..."; deleted "Always frame price differences as discounts" bullet; new callout "Why set only one price when you can set many prices instead?..." with "student discount"/"non-student premium" framing; "group" → "segment" (2x)
- **Slide 35 (console skimming):** replaced gameconsolepriceskimming.png with console_prices.png (vertically trimmed from 960×540 → 960×388 via ImageMagick; original backed up as console_prices_original.png); new Ars Technica archive URL (2026-02-11 capture); callout "Game consoles used to use price skimming to price discriminate between gamers, but that appears to have stopped. Why?"; pared source text
- **Slide 36 (Beware a price war):** "Competitor will notice" → "may notice"; callout → "Why does cost secrecy usually help to avoid price wars?"
- **Deleted old Dynamic pricing slide**
- **Slide 37 (Price Elasticity of Demand, first):** added `{.smaller}`; reorganized bullets (scale-free moved second; inelastic/elastic merged into one line); added "Typically becomes more negative as price falls" and "because a change in ln(X) is approximately equal to the % change in X"; replaced sub-bullets with three "Elasticity calculation depends on method" sub-bullets; new callout "Revenue is maximized at elas.=-1..."; Q → q and P → p throughout
- **Slide 38 (CE special case):** Greek letters α/β; unified sub-bullets "Need exogenous... otherwise, β should be interpreted as correlation"; unified main bullet with "but if C.E. shape is incorrect..."; rewrote callout "C.E. demand is popular because it is easy. However, C.E. assumes that customers with different wtp all have the same price sensitivity..."; replaced `*` with juxtaposition in CE formula; Q → q and P → p throughout
- **Slide 39 (CED vs. het. MNL):** LHS converted to lowercase q, p; wrote q(p) in CED equation; removed ε (used β throughout); rewrote "Optimal price" bullet to "You can show that maximizing π=(p-c)q(p) yields optimal price p*"; RHS removed _j subscripts; c without q(p) argument; `[]` → `()`; removed _m subscripts from grid-search argmax; title "heterogeneous MNL" → "het. MNL"; "Heterogeneous MNL Demand Model" → "Het. MNL Demand Model"; callout rewritten and extended to explain grid search + note about using grid search to test CE assumption; toolkit.png moved from old slide 40, resized 120px → 60px, positioned absolute bottom-right near slide number
- **Slide 40 (renamed "Considerations & Extensions"):** replaced all math/grid-search content with three prose bullets (marginal cost locally constant, demand model predicts competitor response, multiproduct pricing via sum across products); removed `.smaller` to use regular font; callout "We'll optimize product line pricing in week 7"
- **Slide 43 (Competition):** deleted SUGGESTION-prefixed callout entirely
- **Slide 44 (Recap):** first bullet condensed "value pricing, competitor price matching, and cost-based pricing. All 3 are incomplete" → "competitor price matching and cost-based pricing. Both are incomplete"; "most common" → "most common and easiest"
- **Slide 45 (Going further):** updated "More than a Penny's Worth" link from doi.org to Strulov-Shlain Google Drive URL

Source-text corrections (end of session):
- Yang and Lee (2014) → Yang and Lynn (2014)
- Frederick et al. (2013) → Frederick et al. (2014)
- Thaler (2008) empty-URL → Google Drive link (note: year 2008 is questionable; classic beach-beer study is Thaler 1985 in Marketing Science — flagged)

New images added:
- `images/huberputo.png` (960×540)
- `images/decoy-examples.png` (960×540)
- `images/console_prices.png` (960×388 after trim; `console_prices_original.png` backup preserved)

Global changes:
- Removed `\times` multiplication symbol via juxtaposition (replace_all across deck)

Slide counts:
- Started session: 53 PDF pages (corresponding to ~51 slides)
- Ended session: 45 PDF pages (corresponding to ~45 slides)
- Net reduction of ~8 slides and 1 overflow fix

Commands run:
- `quarto render mgt100-06.qmd` after every edit
- `chromium --headless --print-to-pdf=... --virtual-time-budget=180000 --run-all-compositor-stages-before-draw "file://.../mgt100-06.html?print-pdf"` after every edit
- `pdftoppm -f N -l N -r 100 mgt100-06.pdf /tmp/... -png` for per-slide screenshot verification
- `convert` (ImageMagick) with `-trim -fuzz 5% info:` then `-gravity north -chop 0x12 -gravity south -chop 0x140` to crop console_prices.png
- `cp` to backup console_prices_original.png before trimming

Verification:
- Every edit followed by HTML re-render + PDF re-render + pdftoppm screenshot of affected slide(s) + Read of PNG to confirm on-slide corner counter, new text presence, and no overflow
- Final PDF: 45 pages, 11.1 MB

Not yet performed:
- GitHub push pending user approval
- Slide numbering in README uses final (end-of-session) corner-counter values; earlier changes (slides 29-30 swap, slides 31-32 combination, slides 34-39 deletion, Dynamic pricing deletion) have shifted downstream numbering

### 2026-04-20, afternoon PT

Task: Interactive line-by-line revision of mgt100-06.qmd driven by user-specified slide-number edits. HTML re-rendered and PDF regenerated after each edit for visual verification via page screenshots.

Slide-level changes (slide numbers refer to the on-slide corner counter; counter DOES include `#` section-header slides — memory updated accordingly):

- **Slide 2 (FTC surveillance):** callout word tweaks — removed "online" and "such", "start doing" → "do", "I think this press release" → "To me, this".
- **Slide 5/6 (Pricing importance):** added "Counterpoint:" prefix to `"Your margin is my opportunity"` bullet; wrapped bullet list in `font-size: 0.85em` div so the callout fits.
- **Slide 9 (Pricing strategies are secret):** removed SUGGESTION prefix; deleted the word "greater" from the callout.
- **Slide 8 (How firms usually set prices):** typo `exogneous` → `exogenous`.
- **Slide 12 (Spann):** callout question shortened "Why do you think that is?" → "Why?"
- **Slide 13 (Value Pricing):** callout trimmed — removed "For new firms and small firms," prefix; "This easy process can" → ", to".
- **Old slide 14 (Pricing Thermometer):** first fixed the squished image (removed forced `height="420px"`; natural 1.29:1 ratio restored), shortened bullets, rewrote callout as "Why do you think EVC exceeds Perceived Value?". Later merged thermometer content into slide 18 (Choosing p) and deleted this slide entirely.
- **Slide 15 (How to calculate EVC):** callout deleted.
- **Slide 16 (EVC tips):** period removed from first bullet; "reconsider product or target customer" → "reconsider target customer and/or value proposition"; "Unquantifiable" → "Qualitative"; callout deleted.
- **Slide 17 (EVC example):** `Batteriser` → `Batteroo Boost` in title and first bullet; callout rewritten to "Batteroo Boost raised almost $400k on [Indiegogo]" with Indiegogo linked to the correct Indiegogo URL.
- **Slide 18 (Choosing p in (cost, EVC)):** converted to 70/30 two-column layout with thermometer image in right column (~25% smaller than old slide 14); fixed LaTeX math — `{z%}` was producing an "extra open brace" error because `%` starts a LaTeX comment, changed to `\cdot z\%$`; added `{.smaller}` class; deleted "when making your judgment" and "What's your exit?" and "of y"; added first bullet "Pricing 'Thermometer:' How much inducement do you give your customer?"; deleted old callout and added new "Why do you think EVC exceeds Perceived Value?" callout.
- **Slide 18 (Why not just ask customers for their WTP?) — after slide 14 deletion:** unified main + sub-bullets into five single-line bullets (Hypothetical bias / They don't know / Strategic bias / Lack of competition / Social desirability/Identity); refined "Strategic bias" wording to "influence firm's operating or pricing decision"; replaced callout with Miller et al. (2011) summary and added Miller et al. source-url link.
- **Slide 19 (Van Westendorp):** typo `my values` → `many values`; callout deleted.
- **Slide 20 (VW output):** deleted callout + Strengths + Limitations; image `vw.jpg` shrunk `height="350px"` → `height="300px"` (~15% smaller); `Min. # of price-refusers` → `Possibly min. # of price-refusers`; new final bullet "These confusing ideas are largely hypothetical and not well validated" (iterated through several wordings).
- **Slide 21 (Conjoint works for pricing too):** deleted; `# Pricing factors` section header shifted up to become slide 21, with three added outline bullets: Human Factors / Economic Factors / Price Elasticity of Demand.
- **Slide 22 (Signals and Perceived Quality):** added `{.smaller}` class; initial callout deleted, then new callout added: "Suppose you offer the best product in the market at the lowest price; will you get 100% market share?"
- **Slide 23 (Price as a signal / Farel):** callout rewritten to "Price can be a powerful signal of quality. Occasionally, we even see reverse-price-wars. Do you think Nadal paid for that haircut?"
- **Slide 24 (Krishna figure):** callout rewritten to summarize what drives perceived price.
- **Slide 25 (renamed Cognitive Costs):** title `Non-monetary costs` → `Cognitive Costs`; sub-bullet arrows replaced with `<br>&ensp;&ensp;= / + / +` markers; image `1click.jpg` 180px → 155px (~15% smaller); new callout about cart abandonment; added source-url [Bertini et al. (2023)](https://archive.ph/Jy3b6).
- **Slide 26 (Perceived prices):** callout rewritten — "How do you evaluate these two pairs of price discounts? How do most consumers evaluate them?"
- **Slide 27 (Left-digit: Demand Effects):** callout rewritten to describe Strulov-Shlain's ground-coffee methodology + "But maybe the shoppers just hadn't had their coffee yet?" quip; added source-url [Strulov-Shlain (2023)].
- **Slide 28 (Left-digit: Lyft rides):** callout rewritten iteratively — final version: "Lyft ran a huge pricing experiment with 21+ million riders. Offer acceptances jumped discontinuously at dollar thresholds."; added source-url [List et al. (2023)].
- **Old slide 29 (tax appeal left-digit):** deleted entirely. Downstream slide numbers shifted down by 1 again.

Slide numbering mechanics (learned this session):
- User refers to slides strictly by the number printed in the slide's bottom-right corner (title slide = 1). Corner counter DOES count `#` section-header slides and all `##` content slides. Memory `feedback_check_all_pages.md` updated to reflect this.
- PDF overflow pages are unrelated to corner-counter slide numbers.

Rendering:
- HTML re-rendered (`quarto render mgt100-06.qmd`) after every edit.
- PDF re-rendered via local http server + Chromium headless with `--run-all-compositor-stages-before-draw` + `--virtual-time-budget=180000`; PDF page count dropped 92 → 56 → 54 as overflows were eliminated and several slides deleted.

Verification:
- Each edit was visually verified by `pdftoppm` extraction of the affected PDF page(s) and inspection of the PNG via the Read tool. Corner-counter slide numbers on the rendered output confirm the mapping used for every edit.

Not yet performed:
- Several older SUGGESTION-prefix callouts on slides further down in the deck remain untouched.
- GitHub push pending user approval.

### 2026-04-19, late afternoon PT

Task: Phase 2 of mgt100-06.qmd revision — applied most of the reformat and agreed-on content additions; left intermission slide and new section header(s) pending user decisions.

Changes performed (mgt100-06.qmd rewritten via Write, HTML re-rendered, PDF not yet regenerated):

1. YAML rewrite to clean-revealjs template (subtitle "MGT 100 Week 6", dual author block with affiliations, dynamic date-format, custom.css, gtag.html, chalkboard).
2. Copied 3 missing image files from `archive_mgt100_content/archive_mgt100images/` into `images/`: `hbs pricing motivation.png`, `krishna 2009 fig1.png`, `wu cosguner mksc 2020 diamond decoy effect.png`.
3. Slide 3 (Costco) revised with in-class editable HTML table (4 `contenteditable="true"` td cells for live typing during discussion; not persisted).
4. Removed all `{.scrollable}` (7 instances); used `{.smaller}` where needed.
5. Converted all `::: aside` blocks (~12) to `::: {.source-url}` (citations) or `::: {.callout-note}` (commentary).
6. Standardized all image sizing: `width="Xin"` / `out-width="\\linewidth"` → `height="Npx"`. Closing decorative images use `.absolute bottom="20" right="20" height="100px"` (scroll/recap/takingoff) or `120px` (competition).
7. Converted over-indented code-block sub-bullets to proper 4-space nested lists across ~8 slides.
8. Converted `#### Limited-data analyses` / `#### Stated-Preference Data` etc. markdown subheaders on the "How firms set prices" slide to bullet groups (per user directive).
9. Reordered EVC flow: Value Pricing → Pricing Thermometer → How to calculate EVC → EVC tips → Batteriser → Choosing p.
10. Renamed two decoy slides to "Decoy effects (baseline)" and "Decoy effects (adding a decoy)" for clarity.
11. Fixed typo `.scollable` → removed entirely with the scrollable sweep.
12. Fixed "Ars Tecnica" → "Ars Technica".
13. Standardized source-url labels: `[orig]` → `[FTC (2025)]`, `[source]` on Costco → `[EBITDA Catalyst]`, `[source]` on camel → `[camelcamelcamel]`, `[Source: Wu & Cosguner (2020)]` → `[Wu & Cosguner (2020)]`, etc.
14. Added 46 SUGGESTION-prefixed callout notes (one per content slide), drawing on existing speaker notes where substantive.
15. Added `# Wrapping up` section header before Competition / Recap / Going further.
16. Added Competition slide per user's custom spec: students compute Apple's best-response A1 price as a function of Samsung's S1 price using `mdat1` and `out10`, plot the best-response curve, mark the response to a specific S1 drop.
17. Added Yavorsky content additions:
    - G1 "Why not just ask customers for their WTP?" (5 biases of stated-preference asking, in text bullet format per user preference)
    - G3 "Price optimization: CED vs. heterogeneous MNL" (two-column side-by-side, inserted between the existing elasticity slides and the grid-search slide)
    - G4 "Decoy effects: counterpoint" (Bogard, Reiff, Caruso, Hershfield 2024, ScienceDirect + SSRN links)
    - G6 "Dynamic pricing" (Uber/Lyft, Wendy's surge menu, Happy Hour, Amazon DVD experiment, etc.)
    - G8: modified existing Anchoring slide to add boba/Yeti example using La Jolla Village Drive (per user edit of Yavorsky's Mira Mesa version)
    - G9: supplemented existing "Signals and Perceived Quality" slide with Yavorsky additions (Super Bowl ads, B-list celebrities)

Commands run:
- `cp <3 source paths> <images/>` — single Bash call, absolute paths, no shell variables
- `quarto render mgt100-06.qmd --to html` — rendered successfully
- `WebSearch` for Bogard et al paper URL (SSRN + ScienceDirect)

Verification:
- `Grep` for `scrollable|width=[0-9]|out-width|scollable|::: aside` in .qmd returns only expected `width="NN%"` matches on column layouts (4 hits across 2 two-column slides).
- `Grep` for `SUGGESTION:` in .qmd = 46 callouts.
- `Grep` for `contenteditable` in rendered .html = 4 (slide 3 table cells).
- Section header count: 2 (`# How firms set prices` existing, `# Wrapping up` new).
- Content slide count: ~49 `##` headings.
- HTML rendered to `mgt100-06.html` with no errors.

Not yet performed (awaiting user decisions):
- Intermission slide (~75% through deck): user rejected all 5 initial proposals; 5 new options proposed (EVC reusable water bottle, left-digit Starbucks, Van Westendorp mini-dataset, pizza non-monetary costs, café decoy identification). User to pick.
- Additional section headers (B.4): user preferred combining subheadings into one new section rather than 3 separate headers. Proposed single `# Pricing factors` before "Signals and Perceived Quality"; user to confirm 1 vs 2 headers.
- PDF re-render: pending intermission + section header decisions to avoid re-rendering twice.
- GitHub push: pending final approval.

### 2026-04-19, afternoon PT

Task: Phase 1 of mgt100-06.qmd revision — produced scoping review file `mgt100-06_review.md` for user to annotate before actual deck edits begin.

Inputs reviewed:
- `mgt100-06.qmd` (current legacy-YAML source, ~46 slides)
- `mgt100_06_script.R` (class script: het MNL, grid search, Samsung line pricing)
- `mgt100-05.qmd` current + GitHub version (closing slide template + week 5 competition style reference)
- Yavorsky `Class 7 - Pricing.pdf` (full 45-slide deck read via PDF page extraction, pages 1-45)
- CLAUDE.md protocol, MEMORY.md entries, prior README change log

Commands run:
- `find /mnt/chromeos/GoogleDrive/MyDrive/ -type f -iname "*hbs*pricing*" -o ...` — located the 3 missing images in multiple archive locations (no TODO markers needed)
- `Read` on Yavorsky PDF (pages 1-20, 21-40, 41-45) for potential content additions
- `Grep` for `Competition` in mgt100-05.qmd (both gdrive and ~/mgt100_repo versions) — identified user's original tight competition style
- `Read` mgt100_06_script.R — identified available objects for competition assignment options

Output: `mgt100-06_review.md` (~380 lines) covering:
- A. Potential bugs / broken refs (3 missing images located in archive, typo, LaTeX directive)
- B. Structural / flow issues (missing Competition slide, `# Wrapping up` header, intermission, section headers; YAML rewrite needed)
- C. Content redundancies & callout issues (zero callouts currently; table of notes → callout source material)
- D. Formatting & template compliance (scrollable removal, aside conversion, image sizing, source-url labels)
- E. Pedagogical observations (Batteriser EVC never shown on slide; VW over-length; missing CED/MNL side-by-side)
- F. Five competition slide options (user picks)
- G. Ten Yavorsky content additions (user picks which to include)
- H. Intermission proposal (~75% through deck, after decoy evidence, "spot the trick" think-pair-share with 4 alternatives)
- Priority summary table
- Open questions requiring user decisions

Verification:
- `mgt100-06_review.md` file exists at `/mnt/chromeos/GoogleDrive/MyDrive/aaaCURRENT/mgt100_shell/mgt100_gh/mgt100-06_review.md`
- Review structure parallels `mgt100-05_review.md` for consistency
- No deck edits made; Phase 1 is a scoping artifact only, awaiting user review before Phase 2 reformat

### 2026-04-18, ~9:55 AM PT

Task: Partial address of mgt100-05_review.md items. User is reviewing the rest manually.

Changes performed (HTML re-rendered; PDF not yet regenerated):

1. Removed two SUGGESTION callouts on the Quidel slides (slide 1 "Segmentation case study: Quidel" and slide 2 with quidel2.png image). User preferred no callouts on these two slides.
2. Moved `## Class script` slide to appear BEFORE the `# Wrapping up` section header, matching the closing sequence in decks 01-03 and in project_closing_slides_template memory note.
3. Source-url standardization (review item D.11): converted 3 generic `[Source]`/`[paper]` labels to more identifying labels.
    - Line 461 (SSA22 demand models): `[paper]` → `[Optimal Price Targeting (MkSc 2022)]` (matching the label already used on the Research question slide, same URL)
    - Line 592 (Sample choice task / Sawtooth): `[Source]` → `[Sawtooth Software]`
    - Line 616 (Conjoint: Smartphones / Conjointly): `[Source]` → `[Conjointly]`
4. Added a new content slide `## MSPE across 12 demand models` between the K-fold cross-validation image slide and Flexibility vs. Parsimony. Review item E.15: connect CV theory to the R script's output.
    - Generated `images/mspe_by_model.png` by running 10-fold CV on the 12 models defined in `scripts/mgt100_05_script.R` (out1 through out12). Result shows MSPE dropping as helpful heterogeneity is added (models 1-11), then jumping back up for out12 (customer_id interactions) — the overfitting teaching moment flagged in the review.
    - CV was run using a LOCALLY-CORRECTED version of `cv_mspe()` in `/tmp/mgt100_cv/run_cv.R` that uses `predict(out, ...)` instead of the script's `predict(model, ...)` at line 398. The user is reviewing this bug separately (review item A); the saved image reflects the corrected computation. If the bug fix is not applied to the script, students running the class script will see different numbers than the plotted MSPE.
5. Re-rendered HTML successfully. Section count in HTML = 60 (one more than the previous 59 after adding the MSPE slide).
6. Added a final bullet to the `## Going further` slide: `[Advanced: Cal Tech Grad class on empirical IO](https://www.its.caltech.edu/~mshum/gradio/ioclass.html){target="_blank"}`. Verified link resolves to Matthew Shum's Econ 223b "Empirical Methods for Industrial Organization" at Caltech. Re-rendered HTML.

Uncertain / flagged for user review:

- Line 67 `[Full article]` (Transsion / Quartz Africa) — could not identify author via WebFetch (archive.org blocked, qz.com returned 403) or WebSearch. Left unchanged.
- Line 368 `[Source]` on the Ex-post evaluations slide (xws2017.png / Google Drive URL) — could not inspect Google Drive link from WebFetch. Filename suggests a three-author paper; left unchanged pending user confirmation.

Not performed (user handling manually per instructions):

- Review items B.1 (intermission), B.2 (Korzybski misattribution), B.3 (orphaned regularization slide), B.4 (SSA22 to taxonomy), B.5 (approach-3 tension), C.7 (Quidel duplication — rendered moot by removing both callouts), C.8 (Takeaways callout), C.9 (generic callout questions), D.10 (overflow slides), D.12 (competition slide bullets vs table), E.13 (conjoint slide consolidation), E.14 (height-weight scatter), A (cv_mspe bug in R script).

Verification:

- Rendered HTML contains the new slide (confirmed via Grep for "MSPE across 12 demand models" → 1 hit).
- Image file `images/mspe_by_model.png` created (77 KB).
- Section count in HTML: 60 (up from 59, consistent with one new slide added).
- Not yet re-rendered PDF, not screenshotted per-page, not pushed to GitHub — waiting for user to finish manual review and request final render + push.

### 2026-03-22, ~5:50 PM PT

**Task:** Audit and fix mgt100-01-v2.qmd, then finalize as mgt100-01.qmd

**Changes performed:**

1. **Re-rendered HTML and PDF** from mgt100-01-v2.qmd; confirmed 44 slides, no page overflows, no broken images, no missing `target="_blank"` attributes.

2. **Moved population growth slide** (originally slide 11) to just after the cancer/alcohol risk slide (slide 6), per instructor request. Verified new order in PDF.

3. **Added linebreak before "Data:"** in the Competition slide callout.

4. **Reduced Competition slide table font size** to 0.7em using a `style="font-size: 0.7em;"` div wrapper.

5. **Updated Goya competition data** to better reflect Liaukonyte, Tuchman & Zhu (2022) findings:
   - Right-leaning counties: baseline ~87, post +56.4% (was ~52% with flat post-period)
   - Left-leaning counties: baseline ~158, post +9.1% (was 0% — incorrectly flat)
   - Both regions now show dissipation in weeks 2-3, matching the paper's finding that the effect disappeared within 3 weeks
   - Updated both `data/goya_sales.rds` and the slide table

6. **Created and then removed a visualization slide** (slide 43 with ggplot line chart of the Goya data). Image `images/goya_competition_viz.png` remains available but is not used in the deck.

7. **Renamed** mgt100-01-v2.qmd → mgt100-01.qmd (done by user); re-rendered as mgt100-01.html and mgt100-01-slides.pdf.

8. **Pushed to GitHub** (`kennethcwilbur/mgt100`): updated .qmd, .html, .rds, plus new template files (custom.css, gtag.html, mathjax-config.js), 11 new images, and the slides PDF.

**Verification:** All changes verified by re-rendering HTML → PDF → screenshot inspection. Final deck: 44 slides, 44 PDF pages.

**Audit items noted (not yet addressed):**
- 9 `<!-- SUGGESTED CALLOUT: review -->` markers remain in AI-drafted callouts (lines 47, 71, 243, 452, 466, 492, 513, 522, 536)
- 3 images at `height="600px"` in column layouts (cancer.png, goya1.png, goya2.png) — render OK but height attribute is misleading
- Several text-only bullet slides lack callout notes per CLAUDE.md Step 4 guidelines

### 2026-03-22, ~6:15 PM PT

**Task:** Push to GitHub and fix rendering

1. Pushed all root-level files, 11 new images, updated `data/goya_sales.rds`, and `mgt100-01-slides.pdf` to `kennethcwilbur/mgt100`.
2. Fixed accidental `index.html` overwrite (restored remote version).
3. Pushed updated `mgt100-01_files/` directory — the new clean-revealjs template requires updated revealjs libs, chalkboard plugin, and renamed CSS files that weren't in the repo.

### 2026-03-25, ~2:00-5:00 PM PT

**Task:** Reformat and revise mgt100-02-v2.qmd, finalize as mgt100-02.qmd, push to GitHub

**Changes performed:**

1. **Title** updated to "Segmentation, Mapping, Text, LLMs"
2. **Slide 50 (The Challenge):** Fixed LaTeX rendering — wrapped exponent expressions in `$...$` math mode; fixed typo `10,000^5` → `10,000^6` for N=6
3. **Slide 44 (MDS):** Grammar fix "PCA can projects" → "PCA can project"
4. **Slide 40 (PCA FAQ):** Changed "independent component analysis" → "correspondence analysis"
5. **Slides 45-48 (Netzer et al., demand modeling):** Rewrote callouts — added convergent validity, N-grams discussion question, demand modeling vs PCA question
6. **Slide 49 (Text data section header):** Added "Reinforcement Learning" to outline; changed "What are they doing" → "How do they work"; removed "What does it all mean?"
7. **Slide 51 (Embeddings):** Removed SUGGESTION prefix, changed "How is this similar" → "How does this compare", reduced image to 200px
8. **Slide 52 (Cool things about embeddings):** Deleted first bullet, reduced image, rewrote callout with word-math examples (Paris, dollar, Google, Triton), added Mikolov et al. and Pennington et al. source links
9. **Slides 53-54 (Concept Space, Sentences):** Moved after slide 52; removed titles; wrote new callouts explaining 13-dim → 2-dim reduction and autoregressive model question
10. **Slide 55 (Many ways to encode embeddings):** Updated source links — added Boykis (2025)
11. **Slide 56 (RLHF) — NEW:** Added RL diagram, two bullets defining RL and HF, dog-training callout with discussion question, OpenAI and Raschka source links
12. **Slide 57 (How LLMs work) — NEW:** Replaced old 7-step description with corrected version: training summary (pre-train + RLHF), 5-step inference loop (embed, attend, sample, append, repeat), reasoning callout, three Karpathy/Vaswani source links
13. **Deleted slides:** Old "LLMs: Given a prompt" 7-step slide, "What LLMs Can and Can't Do", "What happens next?"
14. **Slide 58 (Class script):** No changes
15. **Slide 60 (Competition):** Rewrote as K-Means clustering assignment with survey data; "the students" → "respondents"; added callout
16. **Slide 61 (Recap):** Rewrote bullets — new first bullet "Customer needs best predict customer behavior, not demos"; added K-Means, embeddings, and LLM bullets; fixed "aide" → "aid"; added `{.smaller}`
17. **Slide 62 (Going further):** Added Applied Causal Inference book link; removed callout
18. **Renamed** mgt100-02-v2.qmd → mgt100-02.qmd; removed v2 files
19. **Pushed to GitHub** (`kennethcwilbur/mgt100`): .qmd, .html, -slides.pdf, .pdf, R script, `mgt100-02_files/`, 3 new images (firefox_usertypes_all.png, gbk_segmentation.png, rl_diagram_transparent_bg.png)

**Verification:** All 62 slides screenshotted from PDF and visually inspected. All 35 referenced images confirmed loading (HTTP 200) on GitHub Pages. HTML slides, PDF, and R script all accessible at kennethcwilbur.github.io/mgt100/.

**Note for remaining decks:** Index page links to `mgt100-XX.pdf` but rendered PDFs use `mgt100-XX-slides.pdf`. Each deck needs a copy with both names (or update index.html). Also, slide 60 "HERE" still needs a download URL for the class survey data.

### 2026-03-27, ~9:00-10:00 AM PT

**Task:** Apply clean-revealjs template to mgt100-03.qmd (Demand Modeling, Week 3)

**Changes performed:**

1. **YAML rewrite** — replaced old `revealjs` format block with clean-revealjs template (title bolded, structured author block with HTML links, `date: today`, dynamic date-format, chalkboard, custom.css, gtag.html)
2. **6 `::: aside` → `::: {.source-url}`** conversions — Wright (1928) ×2, theory paper footnote, Pattnaik (surge pricing), shrinkflation links, simulation source code
3. **16 images resized** — all converted from `width="Xin"` to `height="Npx"`. Shared images matched to decks 1 & 2: intermission.jpg (400px centered), scroll.jpg/recap.png/takingoff.png (absolute bottom-right 100px)
4. **~15 code-block sub-bullets → proper nested lists** — converted 8-space/6-space indented text that rendered as code blocks to standard 4-space-indent sub-bullets throughout
5. **2 two-column layouts added** — footsize.png (portrait, slide 27) and surge.webp (portrait, slide 29) placed in `:::: {.columns}` layouts per the portrait-image rule
6. **Malformed link fixed** — shrinkflation Reddit link had `{target="_blank"}` inside URL parentheses
7. **Slide header comment removed** — `## <!--#Intermission-->` → bare `##`
8. **`.scrollable .smaller` preserved** on 2 MNL math slides per user instruction
9. **`{.smaller}` added to 7 slides** to fix overflows: Demand Curves: Challenges, MNL Pros, MNL Cons, 2. Fundamental issue, 10. General model interpretation, 11a. Simulation, Common solutions
10. **Shrinkflation image reduced** from 350px to 280px to fix callout overflow
11. **35 SUGGESTION-prefixed callout notes written** for all `##` content slides, drawing on existing speaker notes where available
12. **Slide elements reordered** to canonical order: title → bullets/image → callout → source-url → notes

**Verification:** Rendered HTML → PDF → screenshotted all 45 pages. All 45 slides render on exactly 45 PDF pages (down from 52 before overflow fixes). No broken images, no cut-off content, all math renders correctly, all two-column layouts display properly, all standard recurring images match deck 1/2 positioning.

**Template updates:** Added "Standard recurring images" table to CLAUDE.md documenting the 5 standard images and their exact markup. Updated two-column layout guidance to specify portrait-image rule.

**Follow-up fixes (same session):**

13. **Competition slide added** (slide 44) with SUGGESTION-prefixed assignment bullets about MNL estimation with smartphone data. `competition.png` absolute bottom-right 120px, matching decks 1 & 2
14. **Typo fixed** in Recap: "extesnible" → "extensible"
15. **Source-url updated** on MNL Pros: identified `Theory_Transfer.pdf` as Andrews, Fudenberg, Lei, Liang, and Wu (2023), "The Transfer Performance of Economic Models"

**Verification:** Re-rendered HTML → PDF. Final deck: 46 slides, 46 PDF pages. Competition slide, typo fix, and citation all verified via screenshots.

**Items for revision:**
- All 36 callouts (including Competition) are marked `SUGGESTION:` and need user review
- Competition slide assignment content is placeholder — needs actual assignment details
- Competition slide needs to be added to all subsequent decks (05, 06, 07, 09, 10, conjoint)

### 2026-03-29, ~1:30-4:15 PM PT

**Task:** Instructor-guided revision of mgt100-03.qmd callouts, bullet text, and slide content

**Changes performed:**

1. **Slide 2**: Removed 4th paragraph from callout; merged 2nd and 3rd paragraphs
2. **Slide 4**: Rewrote callout with Wright biographical context and discussion question; removed ", describing a causal relationship between P and Q" from source; increased image from 380px to 420px
3. **Slide 5**: Renamed "Inverse Demand Curve" → "Demand Curve"; rewrote callout with MR/MC definitions and discussion questions
4. **Slide 6**: Removed SUGGESTION prefix; removed "What is market power? How would we measure it?" sub-bullet
5. **Slide 7**: Removed first bullet and first sub-bullet; added "brand image, consumer trust, certainty" to demand predictors list; rewrote callout about price endogeneity; removed `{.smaller}`
6. **Slide 8**: Renamed "How firms learn demand" → "How firms usually learn demand"; removed SUGGESTION prefix; removed "Best practice: triangulation" bullet
7. **Slide 9**: Added "input costs and competition" to bullet; rewrote callout with two discussion questions
8. **Slide 10**: Removed SUGGESTION; changed "revealed and stated" → "revealed preferences and stated preferences"
9. **Slide 11**: Changed "predictive analytic" → "predictive & prescriptive analytic"; rewrote callout; later removed "To be fair..." sub-bullet
10. **Slide 12**: Added "Hayashida (2026) : Perishable food" to evidence list
11. **New slide 13 (McFadden & BART)**: Added two-column slide with McFadden portrait and biographical bullets about BART research; callout about MNL introduction; later replaced photo with younger portrait from hetwebsite.net; changed "simulated" → "predicted"; added Ken Train note; moved after Multinomial Logit section header
12. **Slide 15**: Deleted first two bullets (moved to McFadden slide); rewrote callout with BART prediction table discussion; added confidence interval question
13. **Slides 16-17**: Deleted callouts from math slides
14. **Slide 18**: Deleted callout
15. **Slide 19**: Changed "lik." → "likelihood function"; deleted callout
16. **Slide 20**: Deleted callout
17. **Slide 21**: Changed "utility" → "income-constrained utility"; rewrote callout about MNL popularity and microfoundations
18. **Slide 22**: Added "market share functions become harder to calculate"; rewrote callout with Box quote
19. **Slide 23**: Expanded IIA example wording; added "product intercepts in utility"; removed "(why?)"; rewrote callout explaining equal utility inference
20. **Slide 24**: Rewrote callout referencing heterogeneous demand modeling
21. **Slide 26**: Rewrote callout with Wright S&D identification argument; added "100 years" note; reduced image to 380px with 0.75em callout font
22. **Slide 27**: Changed "Demand model is" → "By definition, demand model is"; "may correlate" → "typically correlate"; rewrote callout comparing data science and econometrics; "less focused on action" → "more focused on inference than action"
23. **Slide 28**: Deleted callout
24. **Slide 29**: Consolidated sub-bullets; deleted callout
25. **Slide 30**: Made Luka image smaller (180px) and absolute-positioned bottom-right; removed SUGGESTION
26. **Slide 31**: Rewrote callout about automated pricing and endogeneity
27. **Slide 32**: Rewrote callout about shrinkflation and price measurement; added Janssen and Kasinger (2025) source
28. **Deleted slide 33** (Multiple determinants); renumbered 9→8, 10→9, 11a→10a, 11b→10b
29. **Slide 33 (new)**: Rewrote callout about "Problem of Multiple Determinants"
30. **Slide 34**: Multiple edits — "use data to estimate", math notation for q/p/x, "depends on $e$", math for corr(p,e), "typically unreasonable to expect"; callout: "great place to ask questions"
31. **Slide 35**: Changed to "firm 1 uses", math notation for p2/corr; "OLS model's"; rewrote callout about competitor identification; increased callout font
32. **Slide 36**: Rewrote callout: "Scroll to read the full script"
33. **Slide 37**: Rewrote callout: "Scroll to compare price1 estimates"
34. **Slide 38**: Rewrote callout comparing the two demand curves
35. **Slide 39**: Rewrote callout about Price2 deciles
36. **Slide 40**: Deleted callout; merged sub-bullet onto line 6
37. **Slide 41**: Removed SUGGESTION prefix
38. **Slide 42**: Changed sub-bullet to "If time, explain how you might resolve it"
39. **Slide 45 (Competition)**: Replaced content with price endogeneity description assignment
40. **Slides 46-47**: Removed `{.smaller}` for larger font

**Verification:** Rendered HTML → PDF. Final deck: 47 slides, 47 PDF pages. All callouts reviewed and approved by instructor.

**New image added:** `images/daniel_mcfadden.jpg` (portrait from hetwebsite.net)

### 2026-04-02, ~5:45-6:15 PM PT

**Task:** Push recent changes to GitHub

**Changes performed:**

1. **Installed rsync** (`sudo apt-get install rsync`) — required by `scripts/push_to_github.sh` but not previously installed on this machine.
2. **Fixed push script** (`scripts/push_to_github.sh`):
   - Added `--exclude='*.gslides'` to rsync excludes — Google Workspace native files can't be read via the FUSE filesystem and caused rsync exit code 23.
   - Changed rsync from `-av` (verbose) to `-a` (quiet) — verbose output was ~40K characters, causing tool truncation that hid the git commit/push results.
3. **Pushed to GitHub** (`kennethcwilbur/mgt100`), commit `85ce571`: 28 files changed including updated mgt100-02 deck (.qmd, .html, .pdf), re-rendered mgt100-03.html, three updated R scripts, new images (scree.png, semanticspace.png), updated pinktax.png, and index.html.

**Verification:** Cloned repo after push and confirmed commit `85ce571` contains all expected changes. Re-ran push script and confirmed "No changes to push" (local and remote match).

**Notes:**
- 3 pre-existing broken image references flagged by script: `hbs pricing motivation.png`, `krishna 2009 fig1.png`, `wu cosguner mksc 2020 diamond decoy effect.png` — these are in unreformatted decks (05+) and were not addressed.
- 2 `.gslides` files in `images/` are now excluded from push (harmless, can't be used on GitHub).

### 2026-04-10, ~6:30-7:15 PM PT

**Task 1:** Diagnose and fix mlogit "non-conformable arguments" error in mgt100_03_script.R

**Root cause:** `dfidx()` creates a broken object when its input is a tibble. Tibble's `[` method returns a 1-column tibble for single-column extraction (instead of a vector), causing matrix dimension mismatches inside `mlogit()`. The previous session's diagnosis (stale serialized dfidx objects) was incorrect.

**Fix:** Wrapped all three `dfidx()` calls in `as.data.frame()`:
```r
mdat1 <- dfidx::dfidx(as.data.frame(sub1), choice = "choice", shape = "long", chid.var = "customer_id")
```

**Verification:** All 4 mlogit model specifications across all 3 cohorts run successfully.

**Task 2:** Edit slide 48 (Competition) in mgt100-03.qmd

- Changed "brand intercepts" → "phone dummies"
- Added "Follow good visualization practices from week 1." to callout

**Verification:** Re-rendered HTML and PDF. 50 slides, 50 PDF pages confirmed.

**Task 3:** New GitHub push workflow

Replaced the rsync-based `push_to_github.sh` approach with a persistent local git clone at `~/mgt100_repo`. New workflow:
1. `git pull` in `~/mgt100_repo`
2. Copy changed files from Google Drive
3. Check for new/updated images
4. Verify all images referenced in pushed HTML files exist in `~/mgt100_repo/images/`
5. Commit and push

**Motivation:** Old rsync approach was too slow, error-prone, and silently overwrote newer repo versions with stale Google Drive files. New approach uses standard git, which rejects pushes that would overwrite unseen remote changes.

**Pushed to GitHub** (`kennethcwilbur/mgt100`), commit `9c2648f`: mgt100-03.qmd, mgt100_03_script.R, mgt100-03.html, mgt100-03.pdf, plus 4 new images (corr_vs_caus_hires.png, corr_vs_caus.pdf, feldman_revenue.png, tmall_example.png).

**Cleanup:** Deleted `~/mgt100_temp` (old push script staging dir) and `~/demo-repo` (unused test repo).

### 2026-04-16, ~5:00-6:15 PM PT

**Task:** Initial revision of mgt100-05.qmd (Heterogeneous Demand Modeling, Week 5)

**Status: DRAFT — needs instructor review of all SUGGESTION-prefixed callouts and new content before push.**

**Changes performed:**

1. **YAML rewrite** — replaced old `revealjs` format block with clean-revealjs template (title bolded, structured dual-author block with HTML links for Wilbur + Yavorsky, `date: today`, dynamic date-format, chalkboard, custom.css, gtag.html)
2. **4 `::: aside` → `::: {.source-url}`** conversions — Transsion links, SSA22 paper, conjoint truck source, UberPOOL white paper
3. **~30 SUGGESTION-prefixed callout notes written** for all `##` content slides, drawing on existing speaker notes where available (user preference: use speaker notes as source material for callouts)
4. **~20 images resized** — all converted from `width="Xin"` to `height="Npx"`. Shared images matched to decks 1-3: scroll.jpg/recap.png/takingoff.png/competition.png (absolute bottom-right)
5. **~15 code-block sub-bullets → proper nested lists** — converted 8-space indented text that rendered as code blocks to standard 4-space-indent sub-bullets
6. **3 broken links fixed** — Digital Default Frequencies slide had 3 links with missing closing `}` in `{target="_blank"}`
7. **`.scrollable` removed** from 3 slides (discrete het, continuous het, individual params), replaced with `{.smaller}`
8. **`{.smaller}` added** to 10 slides to prevent overflow: MNL Demand, Het Demand Intuition, How to choose?, Model specification, How do we answer?, Business case, Choosing Product Attributes, Approach, plus existing .smaller slides
9. **Slide elements reordered** to canonical order: title → bullets/image → callout → source-url → notes
10. **MNL Demand rendering bug fixed** — mixed numbered/bulleted sub-list caused blank slide; converted numbered items to regular sub-bullets
11. **Numbered sub-list issue** — Pandoc/Quarto can fail to render slides when numbered sub-items (1. 2. 3.) are mixed with bulleted sub-items (- ) at the same indent level under a parent bullet. Avoid mixing these.

**New slides added (10):**

12. **Heterogeneity: Nested Levels of Generality** — table showing Base MNL → Discrete → Continuous → Individual as nested special cases, with utility equations and nesting relationships
13. **Regularization: Lasso, Ridge & Elastic Net** — explains L1/L2/combined penalties with math, tuning parameter λ, and cross-validation for λ selection
14. **Flexibility vs. Parsimony** — Yavorsky bias-variance visual (extracted from Class 6 slide 31) showing MSE = Bias + Variance tradeoff
15. **Conjoint: Energy Drinks** — Yavorsky example (extracted from Class 6 slide 18) showing realistic shelf-display conjoint survey from SKIM Group / 2023 Sawtooth Conference
16. **Conjoint: Smartphones** — Yavorsky example (extracted from Class 6 slide 19) showing smartphone conjoint with attributes/levels from conjointly.com
17. **Conjoint: Politics** — Yavorsky example (extracted from Class 6 slide 24) showing Thomadsen, Zeithammer & Yao (2023) study of how abortion importance changed after Dobbs decision
18. **Major Benefits of Conjoint** — 4-point summary from Yavorsky: causal, any market scenario, new products, faster/cheaper
19. **Conjoint Market Simulator** — Yavorsky slide (extracted from Class 6 slide 22) showing Conjointly software UI with share/revenue math
20. **Competition** — het MNL estimation with retrodiction: estimate 3+ specs on mdat2/mdat3, use `cv_mspe()` for 10-fold cross-validation to select model, visualize winning model's predictions
21. **Class script** — added "Cross-validation to compare models" bullet

**New images added (5):**

- `images/conjoint_energy_drinks.png` — cropped from Yavorsky Class 6 slide 18
- `images/conjoint_smartphones.png` — cropped from Yavorsky Class 6 slide 19
- `images/conjoint_politics.png` — cropped from Yavorsky Class 6 slide 24
- `images/conjoint_market_simulator.png` — cropped from Yavorsky Class 6 slide 22
- `images/flexibility_vs_parsimony.png` — cropped from Yavorsky Class 6 slide 31

**Verification:** Rendered HTML → PDF. Final deck: 59 slides, 59 PDF pages, 8MB, 3:2 aspect ratio (866x578 pts). All overflows resolved.

**Items for instructor review:**
- All ~30 callouts are marked `SUGGESTION:` and need review/approval
- Competition slide content (uses mdat2/mdat3 with cv_mspe() for retrodiction-based model selection)
- Yavorsky extracted images may need cropping adjustments (extracted at 200 DPI from full PDF pages)
- No content was dropped from the original deck — all existing text preserved verbatim
- Not yet pushed to GitHub

**Context for next session:**
- Deck 3 (Week 3) covers MNL in depth: derivation, estimation, goodness-of-fit, IIA, price endogeneity. Deck 5 was revised with awareness that students already know this material, so MNL review is brief.
- The CLAUDE.md protocol in mgt100_gh/ has 11 steps for reformatting; all were applied.
- Yavorsky's analogous decks are at `../../yavorsky slides 2026Winter/Class 5 - Demand.pdf` and `Class 6 - Heterogeneity.pdf`
- The R script at `scripts/mgt100_05_script.R` uses `mlogit()` with colon syntax for interactions (`:segment`, `:total_minutes`) and includes `cv_mspe()` for 10-fold cross-validation
- User feedback: prefers callouts built from speaker notes content; does not want bold text in prose; treats memory entries as strict rules
