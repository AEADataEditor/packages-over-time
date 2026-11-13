// Estimates at the actual (2019) and placebo cutoffs, one panel per specification.
// Shared by regressions.qmd and appendix.qmd. Plot and width are passed in from the page.
export const mainLabel = "Main journals";
export const didLabel = "DiD: P&P, JEL, JEP vs AER, AEJs";

// pooled samples carry their own labels; a single journal's journal FE is the intercept
export function specLabelsFor(j) {
  if (j === mainLabel) return {its_fe: "(1) Journal FE", its_trend: "(2) Journal FE + journal quarterly trends",
                               pkg: "(5) Post × has package"};
  if (j === didLabel) return {did: "(3) DiD (year FE)", did_trend: "(4) DiD + group quarterly trends"};
  return {its_fe: "(6) Before/after (no trend)", its_trend: "(7) Before/after + quarterly trend"};
}

export function estimatePlot(Plot, width, regs, j, {scale = 1, ylabel, title}) {
  const specLabels = specLabelsFor(j);
  const rsel = regs.filter(d => d.Journal === j && specLabels[d.spec]).map(d => ({...d, Spec: specLabels[d.spec],
          kind: d.type === "actual" ? "Actual: published 2019 and later" :
                d.type === "actual (matched window)" ? "Actual, matched window" : "Placebo",
          xk: d.type === "actual (matched window)" ? "2019 matched" : String(d.cutoff),
          est: scale * d.estimate, lo95: scale * d.lo, hi95: scale * d.hi}));
  const specs = Object.values(specLabels).filter(s => rsel.some(d => d.Spec === s));
  const fx = d => specs.indexOf(d.Spec) % 2, fy = d => Math.floor(specs.indexOf(d.Spec) / 2);
  return Plot.plot({
    title, width, height: 280 * Math.ceil(specs.length / 2), marginLeft: 50, marginTop: 30, marginBottom: 45,
    fx: {axis: null}, fy: {axis: null, padding: 0.2},
    x: {type: "point", label: "Cutoff c: post = 1 if published in year c or later",
        domain: ["2017", "2018", "2019", "2019 matched", "2020", "2021"]},
    y: {label: ylabel, grid: true},
    color: {legend: true, domain: ["Actual: published 2019 and later", "Actual, matched window", "Placebo"],
            range: ["#C2611F", "#E8A15E", "#2F6DB5"]},
    marks: [
      Plot.ruleY([0], {stroke: "#888"}),
      Plot.text(rsel.filter((d, i, a) => a.findIndex(e => e.Spec === d.Spec) === i),
                {fx, fy, text: "Spec", frameAnchor: "top", fontWeight: "bold", fontSize: 12, dy: -14}),
      Plot.ruleX(rsel, {x: "xk", y1: "lo95", y2: "hi95", stroke: "kind", strokeWidth: 2.5, fx, fy}),
      Plot.dot(rsel, {x: "xk", y: "est", fill: "kind", r: 4, fx, fy,
               channels: {Years: d => `${d.first_year}–${d.last_year}`, n: "n", Clusters: "n_clusters",
                          "Pre mean": d => (scale * d.mean_pre).toFixed(1), p: d => d.p.toFixed(3)},
               tip: {format: {x: true, fx: false, fy: false, y: d => d.toFixed(2)}}})
    ]
  });
}

// Event study: coefficient on each year of publication relative to the reference year (2018),
// or, for the rolling trend, the deviation of each year from the trend of the three years before it.
export const trendLabels = {
  none: "No trend: year effects relative to 2018",
  linear: "Linear trend in quarters, fitted on 2015–2018",
  quadratic: "Quadratic trend in quarters, fitted on 2015–2018",
  rolling: "Rolling: each year against the linear trend of the 3 years before"};

export function eventPlot(Plot, width, es, j, trend, {scale = 1, ylabel, title}) {
  const d = es.filter(e => e.Journal === j && e.trend === trend).map(e => ({...e,
    kind: e.year >= 2019 ? "Published 2019 and later" : "Published before 2019",
    est: scale * e.estimate, lo95: e.lo == null ? null : scale * e.lo, hi95: e.hi == null ? null : scale * e.hi}));
  return Plot.plot({
    title, width, height: 380, marginLeft: 55, marginBottom: 45,
    x: {label: "Year of print publication", tickFormat: "d", ticks: d.map(e => e.year)},
    y: {label: ylabel, grid: true},
    color: {legend: true, domain: ["Published before 2019", "Published 2019 and later"], range: ["#2F6DB5", "#C2611F"]},
    marks: [
      Plot.ruleY([0], {stroke: "#888"}),
      Plot.ruleX([2018.5], {stroke: "#44546A", strokeDasharray: "4,3"}),
      Plot.ruleX(d.filter(e => e.lo95 != null), {x: "year", y1: "lo95", y2: "hi95", stroke: "kind", strokeWidth: 2.5}),
      Plot.line(d, {x: "year", y: "est", stroke: "#bbb", strokeWidth: 1}),
      Plot.dot(d, {x: "year", y: "est", fill: d => (trend !== "rolling" && d.year === 2018) ? "white" : null,
                   stroke: "kind", r: 4.5, strokeWidth: 2,
                   channels: {Articles: "n_year", p: e => e.p == null ? "reference" : e.p.toFixed(3),
                              "95% CI": e => e.lo95 == null ? "" : `${e.lo95.toFixed(1)} to ${e.hi95.toFixed(1)}`},
                   tip: {format: {x: "d", y: v => v.toFixed(2), stroke: false, fill: false}}})
    ]
  });
}
