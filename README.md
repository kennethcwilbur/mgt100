# MGT 100 Slide Decks

## Change Log

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
