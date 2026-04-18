# MGT 100 Slide Decks

## Change Log

### 2026-04-18, slide 21 edit

**File:** `mgt100-05.qmd`

- Slide 21 (Het demand: Misuse Risks): deleted final bullet ("Adding heterogeneity to a demand model does not resolve price endogeneity...") and deleted callout.
- Re-rendered HTML; verified via grep that both removed strings are absent.

### 2026-04-18, slide 24 edit

**File:** `mgt100-05.qmd`

- Slide 24 (Lil bit of theory): deleted callout ("The targeting problem reduces to classification...").
- Re-rendered HTML; verified via grep that removed string is absent.

### 2026-04-18, slide 22 edit

**File:** `mgt100-05.qmd`

- Slide 22 (# Some evidence): changed "specification" to "heterogeneity specification".
- Re-rendered HTML; verified via grep.

### 2026-04-18, slide 25 edit

**File:** `mgt100-05.qmd`

- Slide 25 (Information sets $X$): trimmed callout to keep only the final question.
- Re-rendered HTML; verified removed text is absent.

### 2026-04-18, slide 26 edit

**File:** `mgt100-05.qmd`

- Slide 26 (Demand Models $M$): moved "All models trained using 5-fold cross validation." out of source-url into a standard callout; citations stay in source-url with `|` separator.
- Re-rendered HTML; verified callout text present.

### 2026-04-18, slide 27 edit

**File:** `mgt100-05.qmd`

- Slide 27 (How do we answer the question?): trimmed callout to "SUGGESTION: Why might these criteria disagree?".
- Re-rendered HTML; verified removed text absent.

### 2026-04-18, slide 27 follow-up

**File:** `mgt100-05.qmd`

- Slide 27: removed SUGGESTION prefix; appended "Pessimists are often right. Optimists are often wealthy." to callout.
- Re-rendered HTML; verified.

### 2026-04-18, slide 25 follow-up

**File:** `mgt100-05.qmd`

- Slide 25: removed SUGGESTION prefix from callout.
- Re-rendered HTML.

### 2026-04-18, slide 28 edit

**File:** `mgt100-05.qmd`

- Slide 28 (SSA22g.png): replaced callout with "Used the degenerate strategies as benchmarks. What do you see?"
- Re-rendered HTML; verified.

### 2026-04-18, slide 27 quotes

**File:** `mgt100-05.qmd`

- Slide 27: wrapped "Pessimists are often right. Optimists are often wealthy." in double quotes.
- Re-rendered; Quarto renders them as smart quotes in HTML.

### 2026-04-18, slide 29 edit

**File:** `mgt100-05.qmd`

- Slide 29 (SSA22h.png): replaced callout with "How did statistical criteria compare to economic criteria?" (SUGGESTION and prior body removed).
- Re-rendered HTML; verified.

### 2026-04-18, slide 30 edit

**File:** `mgt100-05.qmd`

- Slide 30 (SSA22i.png): rewrote callout as "Past purchases on discount are, by far, the strongest predictor of models' targeting decisions. If you want to predict behavior, use past behavior".
- Re-rendered HTML; verified.

### 2026-04-18, slide 31 edit

**File:** `mgt100-05.qmd`

- Slide 31 (Takeaways): deleted callout.
- Re-rendered HTML; verified removed text absent.

### 2026-04-18, slide 30 follow-up

**File:** `mgt100-05.qmd`

- Slide 30 (SSA22i.png): deleted 2nd sentence of callout; enlarged graphic height 400→550px.
- Re-rendered HTML + PDF; verified slide 30 in PDF (pdftoppm page 30) — graphic readable, callout fits, no overflow.

### 2026-04-18, slide 32 edit

**File:** `mgt100-05.qmd`

- Slide 32 (# Conjoint Analysis): deleted ": >10k studies/year (Sawtooth 2008)" from 2nd bullet.

### 2026-04-18, slide 33 edit

**File:** `mgt100-05.qmd`

- Slide 33 (Choosing Product Attributes): removed SUGGESTION prefix from callout; removed inline `style="font-size: 0.75em;"` from callout fence.
- Re-rendered HTML + PDF; slide 33 PDF screenshot shows clean layout with room to spare.

### 2026-04-18, slide 33 follow-up

**File:** `mgt100-05.qmd`

- Slide 33: removed `{.smaller}` from title to enlarge bullet font sizes.
- Re-rendered HTML + PDF; slide 33 PDF screenshot shows larger bullets filling the slide with callout still readable.

### 2026-04-18, slide 35 edit

**File:** `mgt100-05.qmd`

- Slide 35 (Sample choice task): appended ": Trucks" to title.
- Re-rendered HTML; verified.

### 2026-04-18, slide 36 edit

**File:** `mgt100-05.qmd`

- Slide 36 (Conjoint: Energy Drinks) → retitled "Sample Choice Task: Energy Drinks"; callout deleted; moved to precede the Trucks slide (now new slide 35).
- Graphic recreation from Yavorsky deck not performed — source location not found locally; flagged to user.
- Re-rendered HTML.

### 2026-04-18, slide 34/35 follow-ups + pros-cons move

**File:** `mgt100-05.qmd`

- Slide 34: title Implementation→Algorithm; "Choose a"→"Recruit a"; deleted "to estimate attribute utilities" from step 5; step 6: "with cost"→"with projected cost"; "choose product locations"→"choose product attributes".
- Slide 34 earlier: Added Cons bullets 4 and 5 to Conjoint Pros and Cons; deleted "Conjoint Market Simulator" slide; deleted Class Script bullets 2,3,4.
- Slide 35 (Energy Drinks): `height="600px"` was distorting the image when Chromium forced both height and slide-width; switched to `width="100%"` so aspect ratio is preserved at natural 2.44:1.
- Moved "Conjoint Pros and Cons" slide to sit just before "Conjoint: Limitations, Workarounds" (previously was right after Sample Choice Task: Trucks).
- Re-rendered HTML + PDF; verified slides 34, 35, and relocated Pros/Cons via pdftoppm.

### 2026-04-18, slide-34/35/36/37 batch + aspect-ratio rule

**File:** `mgt100-05.qmd` + `images/*.png`

- Slide 34 (Conjoint Analysis Implementation): `i.e.` → `e.g.`; step 1 $K$ and $x_k$ deleted; u_jl and s_j equations x_{jk} → x_j; "predict profits" → "maximize profits"; kept `{.smaller}` after a no-`{.smaller}` test overflowed.
- Slide 34 (later): Step 4 equations regrouped; s_j numerator sum subscript k→l; denominator x_{lk}→x_k and p_l→p_k; joined onto one line (removed `<br>`). Memory list: dropped "8 GB", appended "256 GB".
- Slide 35 (Sample Choice Task: Energy Drinks): deleted source-url; image enlarged to 600px; briefly tried two-column layout with shelf-only re-crop but reverted per user instruction that image aspect ratios must never be changed.
- Slide 36 (Sample Choice Task: Trucks): deleted callout; deleted source-url; image enlarged 350→600px (original aspect ratio preserved).
- Deleted "Conjoint: Smartphones" and "Conjoint: Politics" slides.
- "Major Benefits of Conjoint" → "Conjoint Pros and Cons": 1. Pro: It's experimental, so we get causal effects by design. No endogeneity / 2. Pro: It's purely hypothetical so we can test any scenario we can think of / 3. Pro: It's fast and confidential, and relatively cheap. Callout deleted.
- Saved permanent memory: `feedback_never_change_image_aspect_ratios.md` — never crop/reshape/replace images with different-aspect versions.
- Re-rendered HTML + PDF; verified slides 34, 35, 36, 37 via pdftoppm screenshots.

### 2026-04-18, follow-ups (energy-drinks image, slide 50 delete, title case, slide 34 math, slide 32)

**File:** `mgt100-05.qmd` + `images/conjoint_energy_drinks.png`

- Replaced `conjoint_energy_drinks.png` with a cropped render of page 18 of `/home/kennethcwilbur/Class 6 - Heterogeneity.pdf` (Yavorsky deck), preserving the shelf graphic + attribute list. Also saved raw full-page extract as `images/yavorsky_energy_drinks_raw-18.png`.
- Deleted slide 50 (More Products, Uber product tiers) entirely.
- Applied Title Case to all slide titles that were in sentence case: Segmentation Case Study: Quidel; Heterogeneity by Customer Attributes; Heterogeneity by Segment; Individual Heterogeneity; How to Choose?; Model Specification; How to Evaluate Overfitting?; Cross-Validation Algorithm; Non-Random Holdout Evaluations; Het Demand: Misuse Risks; Some Evidence; Research Question; Lil Bit of Theory; Information Sets $X$; How Do We Answer the Question?; Conjoint Analysis Implementation; Sample Choice Task: Trucks; Case Study: UberPOOL; Business Case Was Clear! But ...; Maxdiff Results; Class Script; Wrapping Up; Going Further.
- Slide 34 math edits: deleted the "Beware: p is price..." sub-bullet; rewrote equations to $u_{jl}=x_{jk}\beta_l-\alpha_l p_j+\epsilon_j$ and $s_j=\frac{\sum_k x_{jk}\beta_l-\alpha_l p_j}{\sum_l\sum_k x_{lk}\beta_l-\alpha_l p_l}$ (deleted $\sum_k$ in utility only; U_j→u_{jl}; P_j→s_j; \beta_k→\beta_l; \alpha_k→\alpha_l globally).
- Slide 32 bullet: changed "optimal product designs" to "optimal product designs and prices".
- Re-rendered HTML + PDF; verified slides 34 and 35 via pdftoppm.

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

### 2026-04-18, ~1:07 PM PT

**Task:** Re-render mgt100-05 HTML from the updated qmd in the local GitHub clone.

**Changes performed:**

1. Pulled latest `main` into `/home/kennethcwilbur/mgt100_repo` (fast-forward `2ff5ce7..c509002`, one-line change in `index.html`).
2. User supplied updated `mgt100-05.qmd` and recent local changes plus the `_extensions/grantmcdermott/clean/` folder (clean.scss, _extension.yml, mathjax-config.js), which is not tracked in the repo.
3. Rendered `mgt100-05.qmd` to `mgt100-05.html` with Quarto 1.8.25.

**Commands executed:**

```bash
git -C /home/kennethcwilbur/mgt100_repo pull origin main
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Render completed without errors. Output `mgt100-05.html` written at 13:07 (100,869 bytes). Confirmed 31 `images/` references and two occurrences of title `Heterogeneous Demand Modeling` in the rendered HTML. Did not render PDF; did not push to GitHub.

### 2026-04-18, ~1:20 PM PT

**Task:** Edit mgt100-05 slides 8 and 9 callouts, then re-render HTML.

**Changes performed in `mgt100-05.qmd`:**

1. Slide 8 (Mapping Heterogeneity Extensions) callout: replaced "modeling accuracy" with "empirical reasonableness of modeling assumptions", and inserted "Learning structure is very demanding of the data." between the first and second sentences.
2. Slide 9 (Heterogeneity by customer attributes): added a new callout-note with text "We often restrict elements in $\delta$ and $\beta$ to zero, if we think the interaction is unimportant, to avoid overparameterizing the model.", moved the two questions from the final bullet ("What goes into $w_{it}$? What if $dim(x)$ and/or $dim(w)$ is large?") to the end of the new callout, and removed the now-empty final bullet.

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML successfully. Confirmed in `mgt100-05.html`: 1 occurrence of "empirical reasonableness of modeling assumptions", 1 occurrence of "Learning structure is very demanding", 1 occurrence of "overparameterizing the model", and 1 occurrence of "What goes into" (no duplicate of the moved bullet). Did not render PDF; did not push to GitHub.

### 2026-04-18, ~1:30 PM PT

**Task:** Edit mgt100-05 slide 10, then slide 9, re-render after each.

**Changes performed in `mgt100-05.qmd`:**

1. Slide 10 (Heterogeneity by segment):
   - Deleted the second sub-bullet under the first bullet ("We take usage variables as best available proxies for customer needs").
   - First bullet: removed "$N_l$ and", changed "$i=1,...,N$" to "$i=1,...,N_t$", and rewrote "$N=\sum_{l=1}^{L}N_l$" as "$\sum_{l=1}^{L}N_l=N_t$".
   - Deleted the "Alternatively, it is also possible to estimate segment memberships" bullet and its two sub-bullets (Pro/Cons).
   - Added a new callout-note with user-provided text: "Alternatively, it is possible to estimate segment memberships, rather than supplying them via kmeans. Pros: We don't have to pre-specify segment memberships. Cons: Noisy, so we need a lot of data to do this well."
2. Slide 9 (Heterogeneity by customer attributes): added a new sub-bullet under the "Assume $\beta=\delta w_{it}$..." bullet, reading "Suppose $x_{jt}$ is 1x5, and $w_{it}$ is 2x1, then $\delta$ would be 5x2".

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```
(run twice: once after slide 10 edits, again after slide 9 edit)

**Verification:** Re-rendered HTML successfully both times. Grep checks in `mgt100-05.html`: "supplying them via kmeans" (1), "best available proxies" (0 — deleted), "have to define the segment" (0 — deleted), "kmeans segments based on 6 usage variables" (1 — first sub-bullet preserved), "i=1,...,N_t" and "sum_{l=1}^{L}N_l=N_t" present in the MathJax source, "would be 5x2" (1 — new slide-9 sub-bullet). Did not render PDF; did not push to GitHub.

### 2026-04-18, ~1:55 PM PT

**Task:** Slide 10 font size adjustment; slide 8 callout font smaller; slide 11 title rename, promote and rewrite bullet.

**Changes performed in `mgt100-05.qmd`:**

1. Slide 10 (Heterogeneity by segment): removed `{.smaller}` class from the slide header to make the body font larger (better use of space).
2. Slide 8 (Mapping Heterogeneity Extensions) callout: added `style="font-size: 0.85em;"` to shrink the callout text slightly.
3. Slide 11: changed title from `## 3. Individual demand parameters {.smaller}` to `## Individual heterogeneity {.smaller}`; promoted the second sub-bullet of the "Typically..." bullet to a top-level bullet (removed its 4-space indent); within that promoted bullet, replaced the phrase `estimate $F$ but` with `estimate every $\alpha_i$ and $\beta_i$,`.

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
# PDF verification round:
python3 -m http.server 9876 --directory /home/kennethcwilbur/mgt100_repo &
chromium --headless --disable-gpu --no-sandbox --run-all-compositor-stages-before-draw \
  --print-to-pdf=/home/kennethcwilbur/mgt100_repo/mgt100-05-check.pdf --no-pdf-header-footer \
  "http://localhost:9876/mgt100-05.html?print-pdf"
pkill -f "http.server 9876"
pdftoppm -f 1 -l 28 -r 60 mgt100-05-check.pdf scan -png
```

**Verification:** Re-rendered HTML after edits. For slide 10 font change, rendered PDF and inspected PDF page 16 (= slide 10 "Heterogeneity by segment"): content now uses a larger body font and the callout fully fits on the slide, no overflow. For slide 8 callout shrink, HTML confirms `font-size: 0.85em` attribute is applied. For slide 11, HTML grep confirms "Individual heterogeneity" title and the new "estimate every $\alpha_i$ and $\beta_i$" bullet. Cleaned up temporary PDF and PNG verification artifacts. Did not push to GitHub.

**Flag for user:** After promoting slide-11 sub-bullet 2 with the new text, its content ("estimate every $\alpha_i$ and $\beta_i$, that is very very data intensive") substantially duplicates sub-bullet 3 ("In theory, we can estimate all $(\alpha_i,\beta_i)$ pairs ... Most data intensive"). Sub-bullet 3 is now nested under the new promoted bullet by markdown indentation. User may want to consolidate or remove one of them.

### 2026-04-18, ~2:05 PM PT

**Task:** Slide 12 ("How to choose?") edits.

**Changes performed in `mgt100-05.qmd`:**

1. Deleted the final two sub-bullets under "Model selection: A Judgment Problem": "Use modeling purpose and constraints as model selection criterion" and "What are our demand modeling objectives?".
2. Changed "covariate selection" to "predictor selection" in the remaining second sub-bullet.
3. Rewrote the callout (dropping the SUGGESTION prefix) to user-provided text: "Use modeling purposes and constraints as model selection criteria. Purposes can include prediction, explanation and decision-making. Constraints can include privacy and ethics. What criteria would you prioritize?"

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "Constraints can include privacy and ethics" (1), "predictor selection" (1), "covariate selection" (0), "demand modeling objectives" (0). Did not render PDF; did not push to GitHub.

### 2026-04-18, ~2:10 PM PT

**Task:** Slide 8 callout wording tweak.

**Changes performed in `mgt100-05.qmd`:** In the slide 8 callout, replaced "learn heterogeneity from data. Learning structure is very demanding of the data. " with "learn heterogeneity structure from data (very demanding). " (preserving the space before "Tradeoff").

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "heterogeneity structure from data (very demanding)" (1) and "Learning structure is very demanding" (0 — removed). Did not render PDF; did not push to GitHub.

### 2026-04-18, ~2:15 PM PT

**Task:** Slide 10 — change $N_l$ to $N_{lt}$ in both places.

**Changes performed in `mgt100-05.qmd`:** Replaced `N_l` with `N_{lt}` globally (2 occurrences, both on slide 10: in the first-bullet equation `\sum_{l=1}^{L}N_l=N_t` and in the last-bullet equation `s_{jt}=\sum_{l=1}^{L}N_l s_{ljt}`).

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms 2 occurrences of "N_{lt}" in MathJax source; no bare "N_l" remains. Did not render PDF; did not push to GitHub.

### 2026-04-18, ~2:20 PM PT

**Task:** Slide 13 (untitled hwscatter image) callout edits.

**Changes performed in `mgt100-05.qmd`:** Removed the `SUGGESTION:` prefix from the slide 13 callout, and deleted the final question sentence ("What modeling choices would change if the goal were medical screening vs. clothing design?"). Callout now reads: "Should you model weight=f(height) or height=f(weight)? Notice the many-to-one correspondence and the discrete nature of height measurements. Ultimately it depends on your purpose."

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "medical screening" (0), "SUGGESTION: Should you" (0), "Ultimately it depends on your purpose" (2 — callout + speaker notes, as expected). Did not render PDF; did not push to GitHub.

### 2026-04-18, ~2:25 PM PT

**Task:** Slide 14 (Model specification) callout rewrite.

**Changes performed in `mgt100-05.qmd`:** Replaced the slide 14 callout with user-provided text: "Imagine you prepared for a quiz by memorizing the textbook but without understanding the material. Could you accurately apply the concepts in unseen settings?"

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "memorizing the textbook" (1). Did not render PDF; did not push to GitHub.

### 2026-04-18, ~2:30 PM PT

**Task:** Slide 11 (Individual heterogeneity) — adjust font size to use space better.

**Changes performed in `mgt100-05.qmd`:** Removed `{.smaller}` class from slide 11 header, so body font is now default size.

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
# PDF verification round (local HTTP server + chromium headless), PDF screenshot of slide 11.
```

**Verification:** Re-rendered HTML and PDF. Inspected the slide 11 PDF page ("Individual heterogeneity"): body font is larger, all four top-level bullets, two sub-bullets, and equations are readable; slide content still fits on one page with modest whitespace at the bottom. Cleaned up temp verification artifacts. Did not push to GitHub.

### 2026-04-18, ~2:35 PM PT

**Task:** Slide 16 (overfitting visualization) — delete callout and enlarge image. Slide 15 (Regularization) — remove boldface.

**Changes performed in `mgt100-05.qmd`:**

1. Slide 16: removed the `::: {.callout-note}` block containing "SUGGESTION: Imagine estimating demand at a B2B firm..."; changed image height from `400px` to `500px`.
2. Slide 15: removed `**...**` bold markers from three terms — `Ridge`, `Lasso`, and `Elastic Net`.

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
# PDF verification round for slide 16.
```

**Verification:** Re-rendered HTML. Slide 16: HTML confirms callout text removed ("Imagine estimating demand at a B2B" = 0) and image reference preserved; PDF screenshot shows the enlarged image fits cleanly on the slide with no overflow. Slide 15: HTML confirms `<strong>Ridge|Lasso|Elastic Net</strong>` = 0 (bold gone), plain "Ridge (L2 penalty)" still present (1). Did not push to GitHub.

### 2026-04-18, ~2:40 PM PT

**Task:** Slide 15 further edits — delete SUGGESTION and first callout sentence, reorder Lasso before Ridge.

**Changes performed in `mgt100-05.qmd`:**

1. Reordered the regularizer bullets so Lasso appears before Ridge (matching the slide title order "Lasso, Ridge & Elastic Net"). Elastic Net remains last.
2. Edited the callout: removed the `SUGGESTION:` prefix and the first sentence ("Why would we want to force some coefficients to exactly zero?"). Callout now reads: "If you have 100 product attributes but suspect only 10 matter, Lasso identifies which ones while Ridge keeps all 100 with smaller weights. How does the choice between Lasso and Ridge relate to your beliefs about the data-generating process?"

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "SUGGESTION: Why" (0) and "Why would we want to force" (0), "If you have 100 product attributes" (1). In the qmd, line 246 now starts "- Lasso (L1 penalty):" and line 249 starts "- Ridge (L2 penalty):" (reorder confirmed). Did not push to GitHub.

### 2026-04-18, ~2:45 PM PT

**Task:** Slide 13 — delete last sentence of callout.

**Changes performed in `mgt100-05.qmd`:** Removed "Ultimately it depends on your purpose." from the slide 13 callout. Callout now reads: "Should you model weight=f(height) or height=f(weight)? Notice the many-to-one correspondence and the discrete nature of height measurements."

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep for "Ultimately it depends on your purpose" returns 1 occurrence (speaker notes only — callout sentence removed as intended). Did not push to GitHub.

### 2026-04-18, ~2:48 PM PT

**Task:** Slide 14 callout — change "memorizing" to "perfectly memorizing".

**Changes performed in `mgt100-05.qmd`:** In the slide 14 callout (line 239), replaced "by memorizing the textbook" with "by perfectly memorizing the textbook". Did not touch the other "memorizing" occurrence in slide-16 speaker notes.

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "perfectly memorizing the textbook" (1). Did not push to GitHub.

### 2026-04-18, ~2:55 PM PT

**Task:** Slide 15 content edits and swap of slides 15 & 16.

**Changes performed in `mgt100-05.qmd`:**

1. Slide 15 body:
   - "Adding predictors improves in-sample fit" → "Adding predictors always improves in-sample fit".
   - "to improve predictions" → "on the theory that simpler models predict better".
   - Ridge sub-bullet: "never to exactly zero" → "seldom to exactly zero".
   - Elastic Net sub-bullet: "Combines variable selection with coefficient stability" → "Combines Lasso and Ridge".
2. Slide 15 callout: deleted the last sentence ("How does the choice between Lasso and Ridge relate to your beliefs about the data-generating process?") and appended "Penalties are assumed as proxies for overfitting, but overfitting is not directly optimized.".
3. Swapped slides 15 and 16 in `mgt100-05.qmd` so that the image-only "overfitting_visualized" slide now appears before the Regularization slide. (After swap, the image-only slide is the new slide 15 and Regularization is the new slide 16.)

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep in `mgt100-05.html` confirms: "always improves in-sample fit" (1), "simpler models predict better" (1), "seldom to exactly zero" (1), "Combines Lasso and Ridge" (1), "proxies for overfitting" (1). In the qmd, `overfitting_visualized.png` now appears on line 244 and `## Regularization...` on line 259 (swap confirmed). Did not render PDF; did not push to GitHub.

### 2026-04-18, ~3:00 PM PT

**Task:** Slide 17 (How to evaluate overfitting?) edits.

**Changes performed in `mgt100-05.qmd`:**

1. First sub-bullet under "Retrodiction = RETROspective preDICTION": changed from "Knowing what happened enables you to evaluate prediction quality" to "Quantifies model's ability to generalize to non-training data".
2. Deleted the final two sub-bullets under "We can even train a model to maximize retrodiction quality": "More approaches: Choose intentionally simple models" and "Penalize the model for uninformative parameters: Lasso, Ridge, Elastic Net, etc.".
3. Callout rewritten to: "Be careful not to confuse retrodiction with prediction. Why not?" (dropped SUGGESTION prefix).

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "generalize to non-training data" (1), "confuse retrodiction with prediction" (1), "Choose intentionally simple models" (0), "Knowing what happened enables" (0). Did not push to GitHub.

### 2026-04-18, ~3:10 PM PT

**Task:** Slides 18, 19, 20 edits plus new callout on slide 15.

**Changes performed in `mgt100-05.qmd`:**

1. Slide 18 (Cross-validation): removed `{.smaller}` class and deleted the callout block ("SUGGESTION: Cross-validation systematically tests...").
2. Slide 19 (K-fold image + bullets): deleted the callout ("SUGGESTION: Why use K=5 or K=10 folds...") and changed "Cross-validation is just one tool" to "Cross-validation is just one criterion".
3. Slide 20 (MSPE across 12 demand models): deleted the entire slide (title, image reference, and callout).
4. Slide 15 (image-only overfitting_visualized, after earlier swap): added a callout with user-provided text "Choosing a model that maximize a single criterion, such as R-square, can lead to bad decisions." Image remained at 500px.

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
# PDF verification round (local HTTP server + chromium headless).
```

**Verification:** Re-rendered HTML and PDF. PDF screenshots: slide 15 has image at 500px + new callout and fits without overflow; slide 18 shows larger default font with all algorithm steps visible and whitespace at bottom; slide 19 shows image + 4 bullets (with "criterion") without callout and fits; slide 20 is gone. Cleaned up temp verification artifacts. Did not push to GitHub.

### 2026-04-18, ~3:15 PM PT

**Task:** Slide 18 title addition and a second slide 20 deletion.

**Changes performed in `mgt100-05.qmd`:**

1. Slide 18: changed title from "## Cross-validation" to "## Cross-validation Algorithm".
2. Slide 20 (now "Flexibility vs. Parsimony", was previously slide 21 before the earlier MSPE deletion shifted numbering): deleted the entire slide (title, `flexibility_vs_parsimony.png` image, and callout).

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "Cross-validation Algorithm" (1) and "Flexibility vs. Parsimony" (0 — slide gone). Did not push to GitHub.

**Flag for user:** Slide 18's first bullet still ends with "Algorithm:" introducing the numbered list. Now that "Algorithm" is also in the title, that trailing "Algorithm:" is redundant. User can confirm whether to remove it from the bullet.

### 2026-04-18, ~3:20 PM PT

**Task:** Slide 20 (Ex-post evaluations / auto paper) rework.

**Changes performed in `mgt100-05.qmd`:**

1. Retitled from "## Ex-post evaluations {.smaller}" to "## Non-random holdout evaluations {.smaller}" (kept `.smaller`).
2. Rewrote the first bullet from "Can a model be robust to major changes in the data-generating process?" to "When are models most robust to major changes in the data-generating process?". Second bullet preserved unchanged.
3. Rewrote the callout (dropping the SUGGESTION prefix) to user-provided text: "This paper evaluated retrodictive performance of various models of auto production and pricing estimated using pre-2008 data after the 2008 gas price shock. It supported the generalizability of microfounded models. In what scenarios are tests like this most valuable?"

**Commands executed:**

```bash
quarto render /home/kennethcwilbur/mgt100_repo/mgt100-05.qmd
```

**Verification:** Re-rendered HTML. Grep confirms "Non-random holdout evaluations" (1), "When are models most robust" (1), "pre-2008 data after the 2008 gas price shock" (1), old "SUGGESTION: This paper tests" (0), old title "Ex-post evaluations" (0). Did not push to GitHub.
