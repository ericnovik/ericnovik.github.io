# Course Restructure Plan — SMaC (Statistics, Math, and Computing)

Branch: `course-restructure-moon-ai`

Goal: make the course **more interesting and useful** by (1) adding a slide on
programming and statistics in the age of AI, (2) replacing the generic "motion
in a straight line" example with a memorable Moon-drop story, and (3) turning
that story into a unifying mission that pays off across multiple lectures.

---

## 1. Key structural insight

The "motion in a straight line" example is the **hidden spine of the whole
course**, not just a Lecture 2 throwaway:

- **L2** [`02-lecture/02-Lecture.qmd:548`](02-lecture/02-Lecture.qmd) — derivatives: position → velocity → acceleration
- **L3** [`03-lecture/03-lecture.qmd:381`](03-lecture/03-lecture.qmd) — integration: "From Velocity to Position Functions" (the inverse)
- **L6** [`06-lecture/06-lecture.qmd:881`](06-lecture/06-lecture.qmd) — linear algebra: the overdetermined system `X β̂ = y` fitting `x = a + b t²`
- **L7** — statistical inference / measurement-error models

Reframing this spine as a single **Moon mission** (infer the Moon's gravity by
dropping a ball) makes it emotionally memorable and gives it a concrete payoff
that recurs four times across the course.

### Why the physics lines up perfectly

Newton's second law on the airless Moon:

```
m · ẍ = m · g   ⇒   ẍ = g            (constant acceleration)
```

Integrate twice (released from rest at the top of the tower):

```
v(t) = g · t
x(t) = ½ · g · t²
```

- The **constant acceleration** derived in L2 (currently the arbitrary "6 m/s²")
  *becomes g* — the very number the mission needs.
- Inferring `g` from the `(tₖ, xₖ)` marks is exactly the **least-squares fit**
  of L6 (regress `x` on `t²`; slope = g/2, so ĝ = 2 · slope).

### Two true, memorable hooks fall out for free

1. **The 1-kg label is a red herring** — mass cancels in `m·ẍ = m·g`.
   That's Galileo's insight, confirmed on the actual Moon by Apollo 15's
   hammer-and-feather drop.
2. **The Moon makes the experiment feasible.** With `g_moon ≈ 1.62 m/s²`,
   a 100 m drop takes `√(2·100/1.62) ≈ 11 s` — leisurely enough to read marks
   with a stopwatch. On Earth it's 4.5 s, too fast. Low gravity is a *feature*.

### Reference numbers (for plots / worked examples)

- `g_moon = 1.625 m/s²` (true, "unknown" value to recover)
- Tower height `H = 100 m`, red marks **every 5 m** → `x = 5, 10, …, 100` (20 marks)
  - 5 m (not 1 m) so the clicks are humanly spaced: 20 clicks over ~11 s, ~one every 0.5 s
- Time at mark k: `tₖ = √(2·xₖ / g)` — first mark `t ≈ 2.48 s`, last `t ≈ 11.09 s`
- Stopwatch / reaction noise ≈ 0.1 s on each reading

---

## 2. Requested change A — Lecture 1: "Programming and Statistics in the Age of AI"

**Placement:** right after *SMaC: Why Bother?*
[`01-lecture/01-Lecture.qmd:342`](01-lecture/01-Lecture.qmd) — it extends
"why learn this?" into "why learn this when AI can do it?" and ties back to the
existing *Some Mistakes Are Silly / Deadly* thread (humans err; now machines err
*confidently, at scale*).

**Draft slide:**

```markdown
## Programming and Statistics in the Age of AI {.smaller}

::: columns
::: {.column width="55%"}
::: incremental
-   LLMs (Claude, ChatGPT, Gemini) write R, derive integrals, and explain
    statistics — use them; they will make you faster
-   But they are *confident*: they hallucinate functions, invent citations,
    and make subtle statistical errors that look completely right
-   AI raises the **ceiling** of what you can build, but you still need a
    **floor** — enough understanding to prompt well, verify output, and
    notice when the answer is wrong
-   The computing, calculus, and probability in this course are exactly the
    skills that let you *supervise* the machine instead of trusting it
:::
:::

::: {.column width="45%"}
![](images/ai-confident-wrong.png)
:::
:::

::: fragment
The goal isn't to beat AI at arithmetic — it's to ask the right question
and know when the answer is wrong.
:::
```

**Asset needed:** one screenshot of a confidently-wrong AI answer
(`images/ai-confident-wrong.png`) — ideally a stats/base-rate example.
TODO: produce a reproducible example.

---

## 3. Requested change B — Lecture 2: the Moon drop

**Replace** the slide at [`02-lecture/02-Lecture.qmd:548`](02-lecture/02-Lecture.qmd)
(*Example: Motion in a Straight Line*) with a two-slide arc, then let the
*existing* derivative slides flow on with the constants reinterpreted as `g`.

**Draft slides:**

```markdown
## A Cruel Joke {.smaller}

A thousand years from now, your friends drug you, load you onto a ship,
and fire it at the Moon. You wake in a spacesuit on the surface: a ship,
a 100 m tower beside you, a metal ball stamped **1 kg**, and a stopwatch.

::: incremental
-   The ship's autopilot has the equations of motion programmed in — but
    it's missing one number: the Moon's gravitational acceleration g
-   Your plan: climb the tower, drop the ball, and record the time it
    passes each **red mark** (spaced 5 m apart, 20 marks down)
-   Each mark has a sensor: as the ball passes, it **flashes a light**, so
    from the top you read the time off your stopwatch at each flash
-   From those (tₖ, xₖ) pairs you will *infer* g — and fly home
:::

## Setting Up the Equation of Motion {.smaller}

::: incremental
-   No atmosphere on the Moon, so the only force is gravity. Newton's
    second law:
$$ m\,\ddot{x} = m\,g \;\;\Rightarrow\;\; \ddot{x} = g $$
-   The mass cancels — *that's why the 1 kg label is a red herring.*
    Galileo's insight, confirmed on the Moon by Apollo 15's hammer & feather
-   Integrating twice (we'll cover integration in Session 3) and dropping
    from rest gives the solution we'll use:
$$ v(t) = g\,t, \qquad x(t) = \tfrac{1}{2}\, g\, t^2 $$
-   So the **acceleration** is the second derivative of position — a
    constant g — which is exactly the unknown the mission needs
:::
```

…followed by a simulated-data plot (noisy stopwatch readings vs. the
`x = ½ g t²` parabola) so the inference target is visual.

**Draft simulation chunk:**

```r
set.seed(12)
g <- 1.625               # true (unknown) lunar gravity, m/s^2
H <- 100                 # tower height, m
marks  <- seq(5, H, by = 5)  # 20 red marks, every 5 m
t_true <- sqrt(2 * marks / g)
t_obs  <- t_true + rnorm(H, 0, 0.1)   # stopwatch / reaction noise
```

The existing derivative algebra (`v = 6t`, `a = 6`) then carries over verbatim
with `6 → g` — minimal rewrite, maximal payoff.

---

## 4. Propagating the Moon thread (the through-line)

To turn the story into a genuine course-long mission, carry it into:

- **L3 (integration)** — currently "From Velocity to Position Functions". Make
  the worked example *integrate ẍ = g twice* to recover `x(t) = ½ g t²`. This is
  the "solution we promised to derive later" in L2.
- **L6 (linear algebra)** — the overdetermined `X β̂ = y` slides
  ([`06-lecture/06-lecture.qmd:881`](06-lecture/06-lecture.qmd)) become "fit the
  Moon-drop data to recover g". Design matrix column is `t²`; slope = g/2.
- **L7 (statistical inference)** — the measurement-error model becomes "how
  uncertain is our estimate of g given noisy stopwatch readings?" This is also
  where the **oxygen / sample-size question** (§6.3) is answered: how many marks
  must we record before the confidence interval for ĝ is tight enough to read g
  to one decimal place, then two?

This is the resolved scope: **full propagation** across L2 → L3 → L6 → L7.

---

## 5. Smaller polish

- Typos across decks:
  - "pricical" → "principal" ([`02-lecture/02-Lecture.qmd:212`](02-lecture/02-Lecture.qmd))
  - "Bionomial" → "Binomial" ([`01-lecture/01-Lecture.qmd:531`](01-lecture/01-Lecture.qmd))
  - "Intergration" / "Ingegration" → "Integration" (L3)
  - "informative true prior" repeated 3× in the L1 IRT slide
- Optional high-value addition: a recurring **"verify the AI"** micro-beat
  (e.g., have an LLM solve the L4 medical-testing base-rate problem, then check
  it with code). Operationalizes the L1 AI slide and the syllabus promise that
  the course "discusses the use of modern AI and LLMs in computational
  statistics."

---

## 6. Decisions & open questions

1. **Scope of the Moon thread** — RESOLVED: **full propagation** through
   L2 → L3 → L6 → L7. The mission is the spine.
2. **Old example** — RESOLVED: **throw away the generic straight-line example
   entirely**; all narrative becomes the Moon mission.
3. **Oxygen / sample-size question (NEW, open as a teaching thread)** — How many
   observations (marks recorded) do we need to estimate g to the **first**
   decimal place? To the **second**? This matters because oxygen is limited, so
   each extra reading has a cost — we want the *fewest* drops that buy the
   required precision. This becomes a recurring question:
   - Posed informally in **L2** when the mission is introduced.
   - Made concrete in **L7** via standard errors / confidence intervals on ĝ.
     With marks every 5 m (20 per drop, spanning the whole tower), the unit of
     cost is the **drop**: a single drop already nails the first decimal
     (CI half-width ≈ 0.012), and ~6 drops reach the second decimal
     (half-width < 0.005). The second decimal costs ~6× the oxygen of the first.
   - Can be explored by simulation (vary the number of drops, watch the CI shrink).
4. **Observation mechanism (NEW, resolved as narrative detail)** — How does a
   person standing on top of the tower see the ball pass a mark? RESOLVED: each
   mark has a sensor that **flashes a light** as the ball passes; the observer
   reads the stopwatch at each flash. (Measurement noise then comes from
   reaction time / flash-reading, ≈ 0.1 s.)

---

## 7. Implementation checklist

- [x] L1: add "Programming and Statistics in the Age of AI" slide after *SMaC: Why Bother?*
- [x] L1: create `images/ai-confident-wrong.png` asset (base-rate confidently-wrong chat mockup; wired into the slide as a two-column layout)
- [x] L2: replace *Motion in a Straight Line* with the *A Cruel Joke* + *Setting Up the Equation of Motion* arc (include the light-flash sensor detail)
- [x] L2: add simulated Moon-drop data plot
- [x] L2: reinterpret the existing derivative slides' constants as `g`
- [x] L2: pose the oxygen / sample-size question informally
- [x] L3: reframe "From Velocity to Position" as integrating `ẍ = g` twice
- [x] L6: reframe overdetermined `X β̂ = y` as recovering `g` from drop data
- [x] L7: reframe measurement-error model as uncertainty in `g`
- [x] L7: answer the oxygen / sample-size question — drops needed for ±0.05 (1 drop) and ±0.005 (~6 drops) CI on ĝ; marks every 5 m
- [x] Remove all remaining generic straight-line narrative
- [x] Fix typos (§5)
- [x] Render each modified lecture to confirm it compiles
- [x] Build the interactive data-collection app (§8) — `apps/moon-experiment/` (vanilla JS + Canvas + Chart.js; click-to-time, live clock, data table, CSV export, time-vs-distance plot with recovered ĝ)

---

## 8. Interactive data-collection app (JS)

Instead of only *simulating* the drop in R, students **collect the data
themselves** in a small browser app. This makes the measurement-error story
visceral: their own reaction time *is* the noise.

### Core interaction

- A ball is dropped from the tower on the Moon and falls under constant
  acceleration `g_moon ≈ 1.625 m/s²` (animation in real, slowed, or scaled time).
- As the ball passes each **red mark** (every 5 m, 20 marks), the mark **flashes a light**.
- The student **clicks the mouse** (or presses a key) at each flash; the app
  records the click timestamp `t_k` against the known mark height `x_k`.
- The recorded `(t_k, x_k)` pairs are the dataset they then analyze in R to
  infer `g` — closing the loop with L2/L6/L7.

### Why it's pedagogically valuable

- The noise is **real and self-generated** (human reaction time ≈ 100–250 ms),
  not `rnorm()` — students *feel* where measurement error comes from.
- Connects directly to the oxygen / sample-size question (§6.3): they can stop
  early and see how few clicks still pin down `g`.
- Natural class activity: everyone runs a trial, pool the data, compare
  individual vs. pooled estimates of `g`.

### Rough requirements / open questions

- **Tech**: vanilla JS + Canvas (or a tiny framework); embeddable in the
  reveal.js deck or standalone. Keep dependencies minimal.
- **Time scaling**: a true 100 m Moon drop takes ~11 s; decide whether to run
  real-time, slow-motion, or let the student set the speed.
- **Data export**: download `(t_k, x_k)` as CSV, or paste into R; optionally a
  "pool the class" mode.
- **Calibration**: log the click latency so students can see their own bias
  (systematic late-clicking) vs. random error.
- **Fairness to the model**: noise should land on `t` (click timing), matching
  the L2 narrative; reconcile with the OLS-on-position framing used in L6.
- **Scope**: MVP = single drop, click-to-time, CSV export. Stretch = class
  pooling, latency calibration, adjustable tower height / gravity.
