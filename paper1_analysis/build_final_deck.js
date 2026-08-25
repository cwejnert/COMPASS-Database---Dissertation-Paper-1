// =============================================================================
// COMPASS Paper 1 — final deck.
//
// Story -> Methodology -> Diagnostics -> Results -> Mechanism ->
// Why, World and every region -> Robustness -> What we can claim.
//
// Nine regions plus World in the regional display. Pacific OECD is out of the
// regional rows (the global label does not describe it) and stays inside the
// World aggregate.
//
// THE PRIMARY AXIS IS ALL CDR -- engineered removal plus land-based removal --
// because the paper asks whether renewables-led mitigation beats CDR-led
// mitigation, and a CDR axis that excludes land names a subset of what it
// claims. The engineered-only axis is carried throughout as the sensitivity,
// and it is the CONSERVATIVE one: same direction, smaller advantage.
//
// Every number here comes from LAND_PRIMARY.rds (V5_land_primary.R): repaired
// scenario keys, labels rebuilt with the published rule and checked exactly
// against the published classification for approach A, strict ten-region World.
//
// Results are reported as RAW LEVELS with a cluster-bootstrap 95% interval on
// the raw DIFFERENCE IN MEDIANS. Cliff's delta is retained only where the
// question is genuinely about rank overlap -- the within-model test. Mortality
// runs on the reporting-complete sample, so no ammonia correction is applied.
//
// Figures F1-F5 come from Z9_century_figs.R and are read from the path below,
// so the deck and the figures are always built off the same result objects.
// Run from the repo root, or set COMPASS_FIG to point elsewhere.
// =============================================================================
const P = require("pptxgenjs");
const p = new P();
p.layout = "LAYOUT_WIDE";                       // 13.33 x 7.5 in

const FIG = (process.env.COMPASS_FIG || "paper1_analysis/figures_century") + "/";

const NAVY="12263A", TEAL="1C7293", WHITE="FCFCFB";
const CMT="2F79BF", RE="C68A20", INK="1F2A36", MUTE="5B6B7B",
      PANEL="F1F5F9", LINE="D9E1E8", GREEN="1B7A4B", RED="A33A3A", GOLD="B7791F";
const F="Calibri", FH="Cambria";
let N=0;

function slide(dark){ N++; const s=p.addSlide(); s.background={color: dark?NAVY:WHITE};
  if(!dark) s.addText(String(N), {x:12.75,y:7.02,w:0.4,h:0.28,fontSize:9,color:MUTE,fontFace:F,align:"right"});
  return s; }

function head(s,kick,title,tag){
  if(kick) s.addText(kick.toUpperCase(), {x:0.62,y:0.42,w:9,h:0.26,fontSize:10.5,bold:true,
    color:(tag&&tag.c)||TEAL,fontFace:F,charSpacing:1.6});
  s.addText(title, {x:0.62,y:0.70,w:12.1,h:0.60,fontSize:23,bold:true,color:INK,fontFace:FH,shrinkText:true});
  if(tag&&tag.t) s.addText(tag.t,{x:11.0,y:0.40,w:1.7,h:0.3,fontSize:9,bold:true,color:tag.c||TEAL,
    fontFace:F,align:"right",charSpacing:1.2});
  s.addShape(p.ShapeType.rect,{x:0.62,y:1.42,w:12.1,h:0.02,fill:{color:LINE}});
}

function section(label,title,sub){
  const s=slide(true);
  s.addText(label.toUpperCase(),{x:0.9,y:2.5,w:9,h:0.3,fontSize:12,bold:true,color:"7FB3C8",
    fontFace:F,charSpacing:2.2});
  s.addText(title,{x:0.9,y:2.9,w:11,h:1.0,fontSize:40,bold:true,color:"FFFFFF",fontFace:FH});
  if(sub) s.addText(sub,{x:0.9,y:4.0,w:10.4,h:1.0,fontSize:14,color:"C7D6E0",fontFace:F,lineSpacing:22});
}

// `ratio` is the figure's NATIVE height/width. Both dimensions are scaled by the
// same factor so the figure is never squashed: it fills the 11.3in text column
// unless that would push it past 4.4in tall, in which case height binds and the
// width shrinks with it. The result is centred either way.
function img(kick,title,file,capA,capB,tag,ratio){
  const s=slide(); head(s,kick,title,tag);
  const r=ratio||0.52, maxW=11.3, maxH=4.4, y=1.68;
  const k=Math.min(maxW, maxH/r), w=k, h=k*r, x=0.62+(12.1-w)/2;
  s.addImage({path:file,x:x,y:y,w:w,h:h});
  const cy=y+h+0.16;
  if(capA) s.addText(capA,{x:0.62,y:cy,w:12.1,h:0.34,fontSize:11.5,bold:true,color:INK,fontFace:F});
  if(capB) s.addText(capB,{x:0.62,y:cy+0.34,w:12.1,h:0.7,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
  return s;
}

function table(kick,title,headers,rows,colW,tag,note,fs){
  const s=slide(); head(s,kick,title,tag);
  const body=[headers.map(h=>({text:h,options:{bold:true,color:MUTE,fontSize:(fs||11)-1.5,
    fill:{color:PANEL},fontFace:F}}))];
  rows.forEach(r=>body.push(r.map((c,i)=>{
    const o={fontSize:fs||11,fontFace:F,color:INK,valign:"top"};
    if(typeof c==="object"){ Object.assign(o,c.o||{}); return {text:c.t,options:o}; }
    return {text:c,options:o};
  })));
  s.addTable(body,{x:0.62,y:1.62,w:12.1,colW:colW,border:{type:"solid",color:LINE,pt:0.5},
    rowH:0.28,autoPage:false});
  if(note) s.addText(note,{x:0.62,y:6.62,w:12.1,h:0.62,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
  return s;
}

function bullets(kick,title,items,tag,note){
  const s=slide(); head(s,kick,title,tag);
  s.addText(items.map(t=>({text:t,options:{bullet:{code:"25AA"},breakLine:true}})),
    {x:0.72,y:1.72,w:11.9,h:4.7,fontSize:14,color:INK,fontFace:F,lineSpacing:26,paraSpaceAfter:8});
  if(note) s.addText(note,{x:0.62,y:6.6,w:12.1,h:0.6,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
  return s;
}

// A region slide: profile strip on the left, mechanism and real-world on the right.
function region(name, verdict, vcolor, stats, mech, real, note){
  const s=slide(); head(s,"Region · why", name, {t:verdict,c:vcolor});
  // Stacked label-over-value: the raw cells carry strings like
  // "6.96 -> 19.21 · 3.88 -> 13.63", which will not fit a right-aligned column.
  s.addShape(p.ShapeType.rect,{x:0.62,y:1.62,w:3.62,h:4.62,fill:{color:PANEL}});
  const rowH = Math.min(0.50, 4.42/Math.max(stats.length,1));
  let y=1.74;
  stats.forEach(r=>{
    s.addText(r[0].toUpperCase(),{x:0.82,y:y,w:3.24,h:0.19,fontSize:7.4,color:MUTE,
      fontFace:F,charSpacing:0.7});
    s.addText(r[1],{x:0.82,y:y+0.175,w:3.24,h:0.24,fontSize:10.5,bold:true,
      color:r[2]||INK,fontFace:F,shrinkText:true});
    y+=rowH;
  });
  s.addText("MECHANISM",{x:4.50,y:1.66,w:4,h:0.24,fontSize:9.5,bold:true,color:TEAL,
    fontFace:F,charSpacing:1.4});
  s.addText(mech,{x:4.50,y:1.94,w:8.22,h:1.95,fontSize:11.5,color:INK,fontFace:F,lineSpacing:17,valign:"top"});
  s.addText("IN THE REAL WORLD",{x:4.50,y:4.02,w:4,h:0.24,fontSize:9.5,bold:true,color:GOLD,
    fontFace:F,charSpacing:1.4});
  s.addText(real,{x:4.50,y:4.30,w:8.22,h:1.95,fontSize:11.5,color:INK,fontFace:F,lineSpacing:17,valign:"top"});
  if(note) s.addText(note,{x:0.62,y:6.42,w:12.1,h:0.66,fontSize:9.5,color:MUTE,fontFace:F,lineSpacing:13});
  return s;
}

// ============================== TITLE ========================================
{ const s=slide(true);
  s.addText("COMPASS Scenario Database · Paper 1",{x:0.9,y:2.0,w:10,h:0.34,fontSize:13,
    bold:true,color:"7FB3C8",fontFace:F,charSpacing:1.8});
  s.addText("Carbon removal or renewables?",
    {x:0.9,y:2.45,w:11.4,h:1.5,fontSize:40,bold:true,color:"FFFFFF",fontFace:FH,lineSpacing:46});
  s.addText("Wellbeing outcomes across nine world regions and two levels of ambition",
    {x:0.9,y:4.05,w:10.6,h:0.5,fontSize:16,color:"C7D6E0",fontFace:F});
  s.addShape(p.ShapeType.rect,{x:0.9,y:4.75,w:2.2,h:0.03,fill:{color:GOLD}});
  s.addText("610 classified scenarios · 10 model families · 3 outcome families · 60 comparisons · cumulative 2020–2100",
    {x:0.9,y:5.0,w:11,h:0.34,fontSize:12,color:"9FB6C4",fontFace:F});
  s.addText("CDR axis = ALL removal — Novel CDR + fossil/industrial CCS + land-based removal",
    {x:0.9,y:5.35,w:11,h:0.34,fontSize:11,color:"7FB3C8",fontFace:F,italic:true});
}

bullets("The answer in one slide","Renewables-led mitigation delivers more wellbeing than CDR-led mitigation",
  ["AT WORLD, SEVEN OF EIGHT CELLS POINT TO HIGH-RE — energy employment, the decent-living gap and the deprived headcount at both ambition levels, and PM2.5 mortality at 1.5°C. The one exception is mortality at 2°C, 4.3 million deaths higher under High-RE. Across regions, 42 of 60 comparisons point to High-RE and 18 to High-CDR.",
   "ENERGY EMPLOYMENT — unanimous. High-RE leads in ALL 20 region × ambition cells, and all 20 clear the interval; no other outcome does either. World net energy employment goes from 228 to 692 job-years per 1,000 at 1.5°C (+204%) and 187 to 471 at 2°C (+153%).",
   "ENERGY DEPRIVATION — leads in 14 of 20 cells. The World decent-living gap closes from 18.43 to 11.62 GJ per capita at 1.5°C (−37%, interval clears) and 15.46 to 11.37 at 2°C (interval does not). The deprived headcount moves the same way. Reverses in Rest of Asia and the Middle East.",
   "AIR-QUALITY MORTALITY — the uneven one, leading in 8 of 20 cells. High-RE avoids 11.11 million deaths at World 1.5°C, and Europe carries it: 4.99 and 3.66 million avoided, a 31% reduction. Africa points the other way at both levels, where PM2.5 is household solid fuel and dust rather than the power mix."],
  {t:"SUMMARY",c:GOLD},
  "Cumulative 2020–2100 for both classification and outcomes. Direction is the claim; intervals are reported as context. Every cell is the two arm medians with a 2,000-replicate cluster bootstrap on the raw difference.");

table("The answer in one slide","World, all three outcomes, both ambition levels",
  ["Outcome","Ambition","High-CDR","High-RE","Difference","95% interval","Change"],
  [[{t:"Energy jobs",o:{bold:true}},"1.5°C","227.60","691.52",{t:"+463.92",o:{color:GREEN,bold:true}},{t:"[+380, +513]",o:{color:GREEN}},{t:"+204%",o:{color:GREEN,bold:true}}],
   [{t:"job-years per 1,000",o:{color:MUTE}},"2°C","186.50","471.15",{t:"+284.65",o:{color:GREEN,bold:true}},{t:"[+181, +337]",o:{color:GREEN}},{t:"+153%",o:{color:GREEN,bold:true}}],
   [{t:"Energy deprivation",o:{bold:true}},"1.5°C","18.43","11.62",{t:"−6.81",o:{color:GREEN,bold:true}},{t:"[+1.72, +9.73]",o:{color:GREEN}},{t:"−37%",o:{color:GREEN,bold:true}}],
   [{t:"gap, GJ per capita",o:{color:MUTE}},"2°C","15.46","11.37",{t:"−4.09",o:{bold:true}},{t:"[−0.56, +7.75]",o:{color:MUTE,bold:true}},"−26%"],
   [{t:"PM2.5 mortality",o:{bold:true}},"1.5°C","426.23","415.12",{t:"−11.11",o:{color:GREEN,bold:true}},{t:"[+3.82, +50.40]",o:{color:GREEN}},{t:"−3%",o:{color:GREEN}}],
   [{t:"million cumulative deaths",o:{color:MUTE}},"2°C","433.60","437.88",{t:"+4.28",o:{color:MUTE}},{t:"[−20.6, +35.9]",o:{color:MUTE}},"+1%"]],
  [2.6,1.2,1.4,1.4,1.7,2.2,1.6],{t:"WORLD",c:GOLD},
  "A positive interval favours High-RE throughout. FOUR OF SIX CELLS CLEAR, and all four favour High-RE: jobs at both ambition levels, deprivation at 1.5°C and mortality at 1.5°C. Both cells that fail are at 2°C, where the two arms differ less in how much combustion remains. Mortality runs on the reporting-complete sample (37 vs 37 at 1.5°C), which is why its arms are smaller.");

bullets("What changed, and why","Four upstream decisions, and what each one buys",
  ["THE CDR AXIS IS ALL REMOVAL — Novel CDR, fossil and industrial CCS, AND LAND-BASED REMOVAL. The question is High-RE against High-CDR, and land-based removal is CDR: a scenario leaning on afforestation is a carbon-removal scenario whether or not the removal is engineered. Restricting the axis to engineered removal would name a SUBSET of the comparison arm. The engineered-only axis is reported throughout as the sensitivity.",
   "THE WINDOW IS 2020–2100 for both classification and outcomes, up from 2020–2050. Jobs numbers are larger in absolute terms and include more of the operational phase rather than only the construction surge.",
   "THE ARMS ARE BALANCED BY CONSTRUCTION: 67/67 at 1.5°C and 238/238 at 2°C. The portfolio rule takes same-size terciles on the same sample and subtracts the same overlap from each, so the arms are necessarily equal. Balanced arms remove one asymmetry from every comparison.",
   "MORTALITY USES ONLY DIRECTLY-REPORTED R10 PRECURSORS — all five of SO₂, NOₓ, BC, OM and NH₃, with no World-to-R10 disaggregation and no ammonia sidecar. This resolves the ammonia reporting asymmetry and the synthetic-regions risk at once, by dropping the scenarios that caused them rather than patching."],
  {t:"THE DESIGN",c:TEAL},
  "The ammonia harmonisation that dominated the previous round is now obsolete: the input rule makes the correction unnecessary, which is a better fix than the one it replaces.");

bullets("How to read this deck","Direction first — these are associations, not causal estimates",
  ["THE QUESTION IS DIRECTIONAL. Does renewables-led mitigation deliver more wellbeing than CDR-led mitigation, at matched climate ambition, at World and R10 level? The primary evidence is WHICH WAY each cell points and how consistently — 42 of 60 to High-RE, and 7 of 8 at World.",
   "SIGNIFICANCE IS REPORTED THROUGHOUT, AS CONTEXT. Every cell carries a 2,000-replicate cluster bootstrap interval on the raw difference in medians, and a ● marks the cells that clear zero. It says how firmly a cell points, which is worth knowing. It is not the claim, and nothing here is presented as a causal effect of choosing one pathway over another.",
   "THE OUTCOMES ARE THREE FAMILIES, NOT FIVE MEASURES. Energy employment, energy deprivation (a gap and a headcount that move together) and PM2.5 mortality. Counting the gap and the headcount separately would report one deprivation finding twice.",
   "AND THE DATABASE ITSELF IMPOSES LIMITS, REPORTED AS SENSITIVITIES IN PART SIX. The two arms are not drawn from the same mix of models, mortality runs on a restricted sample, and some scenarios enter on a renewables value they never reported. None of that changes a headline direction; all of it bounds how firmly each can be stated."],
  {t:"FRAMING",c:GOLD},
  "Cumulative 2020–2100 throughout. Positive ALWAYS favours High-RE, so the two lower-is-better outcomes (deprivation gap, mortality) are sign-flipped.");

section("Part one","Methodology","How each wellbeing outcome is built.");

table("Method","The whole design on one slide",
  ["Step","Choice","Why this and not something else"],
  [[{t:"1. Ambition",o:{bold:true}},"AR6 category → 1.5°C (C1+C2) and 2°C (C3+C4)","Compares pathways reaching the SAME climate outcome. Without it the comparison is just ambition."],
   [{t:"2. CDR axis",o:{bold:true}},{t:"ALL removal — novel CDR + fossil/industrial CCS + land-based",o:{bold:true}},"The question names CDR, so the axis is CDR. Restricting it to engineered removal is reported as a sensitivity, and it is the conservative one."],
   [{t:"3. RE axis",o:{bold:true}},"cumulative renewable capacity","Wind, solar, hydro, geothermal. Nuclear and biomass excluded — biomass is the substrate of BECCS, so counting it would let one scenario score on both axes."],
   [{t:"4. Labels",o:{bold:true}},"top tercile on the focal axis, NOT top tercile on the opposing axis","Scenarios high on both are genuinely both and are dropped; high on neither are neither. Same-size terciles minus the same overlap yields balanced arms by construction."],
   [{t:"5. Window",o:{bold:true}},"cumulative 2020–2100","Full scenario horizon, for both the classification and the outcomes."],
   [{t:"6. Inference",o:{bold:true}},"cluster bootstrap on the raw difference in medians","610 classified scenarios sit in 335 model × scenario-family clusters. 2,000 replicates resampling whole clusters."]],
  [2.0,4.2,5.9],{t:"DESIGN",c:TEAL},
  "One fixed global classification is applied unchanged in every region, so a regional result is never an artefact of relabelling scenarios inside that region.");

table("Method — outcome 1 of 3","Energy jobs: what is counted, where, and when",
  ["Element","Choice","Detail"],
  [[{t:"THE UNIT",o:{bold:true,color:GREEN}},{t:"JOB-YEARS, not jobs",o:{bold:true,color:GREEN}},
    "One job-year = one person employed full-time for one year. A job lasting 30 years is 30 job-years; 30 jobs lasting one year is also 30 job-years. This is a STOCK OF WORK over the whole period, NOT a headcount at any moment — so 687 job-years per 1,000 people over 2020–2100 means roughly 8.6 people in work per 1,000 at any given time, not 687"],
   [{t:"How it is computed",o:{bold:true}},"annual jobs × years, summed 2020–2100",
    "The model gives capacity by technology at each reporting node. Jobs are computed at each node, then integrated across the period. Decadal nodes are treated as representing their decade"],
   [{t:"CONSTRUCTION",o:{bold:true,color:TEAL}},"per GW BUILT in that period · transient",
    "Scales with NEW capacity added, so it appears only while building happens and disappears when it stops. Located where the plant is sited — this work cannot be imported"],
   [{t:"MANUFACTURING",o:{bold:true,color:TEAL}},"per GW BUILT in that period · transient",
    "Also scales with new capacity, but is NOT necessarily located where the plant is. Assigned to the deploying region here, which is the standard AR6 convention and an optimistic one for regions that import turbines and panels"],
   [{t:"OPERATION & MAINTENANCE",o:{bold:true,color:TEAL}},"per GW of STOCK in place · persistent",
    "Scales with installed capacity, not with additions, so it accumulates as the fleet grows and persists for the asset's life. This is the only stream that survives the end of the build-out"],
   [{t:"Fossil side",o:{bold:true}},"extraction · refining · fossil O&M","Subtracted, so the measure is NET. A pathway cannot score by building alone, and one that only shuts down coal scores zero on the renewable term"],
   [{t:"The measure",o:{bold:true}},{t:"renewable job-years − fossil job-years",o:{bold:true}},"Per 1,000 people on a FIXED base-period population, identical in every scenario, so demography cannot move a contrast"],
   [{t:"Grouping",o:{bold:true}},"renewables = wind · solar · hydro · geothermal","Nuclear and bioenergy are tracked separately and BOTH favour High-CDR (−10 and −10 job-years per 1,000 at World 1.5°C). Biomass is excluded from the renewable axis because it is the substrate of BECCS — counting it would let one scenario score on both classification axes"]],
  [2.3,3.0,6.8],{t:"JOBS",c:TEAL},
  "SPATIAL RESOLUTION: jobs are computed per R10 region from that region's own capacity, then summed to World. TEMPORAL: the three streams behave differently over time — construction and manufacturing track the RATE of building and fade, O&M tracks the STOCK and persists. That distinction is what makes the advantage front-loaded and is the mechanism behind the regional ordering.",9.5);

table("Method — outcome 2 of 3","Energy deprivation: how the decent-living gap is built",
  ["Element","Choice","Detail"],
  [[{t:"THE IDEA",o:{bold:true,color:GREEN}},{t:"how far BELOW a decent-living floor",o:{bold:true,color:GREEN}},
    "Not total energy use, and not inequality. A threshold is set for the final energy a person needs for decent living; the gap measures the SHORTFALL beneath it. A region above its floor contributes zero"],
   [{t:"Threshold",o:{bold:true}},"Kikstra et al. 2021, fig 1A · regional",
    "Final energy in GJ/cap/yr needed for decent living: India+ 10, China+ 15, Africa 17, Europe 28, North America 37. Regional because the same service costs different amounts of energy in different climates and settlement patterns"],
   [{t:"Sector split",o:{bold:true}},"residential/commercial · transport · industry",
    "DESIRE shares 6.7 / 11.8 / 3.8 GJ/cap, normalised to 30% / 53% / 17%. The threshold is applied SEPARATELY IN EACH SECTOR, not to the total"],
   [{t:"Efficiency path",o:{bold:true}},"−1.9%/yr on the threshold, floored at 50%",
    "The energy needed to deliver decent living falls over time as service provisioning improves, so the bar drops. Floored at half its 2020 value so it cannot collapse to nothing by 2100"],
   [{t:"THE GAP — step by step",o:{bold:true,color:GOLD}},{t:"gap = max(0, threshold − actual)",o:{bold:true,color:GOLD}},
    "(1) For each sector, subtract that sector's delivered final energy per capita from that sector's threshold. (2) TRUNCATE AT ZERO: if delivered exceeds the threshold the sector contributes 0, never a negative. (3) Sum the three sectors. (4) Sum over years 2020–2100. The result is cumulative GJ per capita of shortfall"],
   [{t:"Why truncation matters",o:{bold:true,color:RED}},{t:"surplus never offsets shortfall",o:{color:RED}},
    "Because negatives are clipped, only sectors where a region falls SHORT can move the measure. Abundant industrial energy cannot compensate for a residential shortfall, and a rich region cannot offset a poor one. This makes it a DEPRIVATION measure rather than a net-adequacy one"],
   [{t:"Second measure",o:{bold:true}},"headcount below threshold (%)","Share of population living below the floor. Moves with the gap in every cell; reported as a within-family check and because it converts directly into people"]],
  [2.3,3.0,6.8],{t:"DEPRIVATION",c:TEAL},
  "WHAT IT CANNOT SEE: the threshold is applied to a REGIONAL AVERAGE, so the measure is a regional aggregate and cannot say who inside a region is deprived. The implied within-region inequality is lower than observed, so it understates deprivation in unequal regions.",9.5);

table("Method — outcome 3 of 3","PM2.5 mortality: from emissions to deaths, and the input rule",
  ["Element","Choice","Detail"],
  [[{t:"THE CHAIN",o:{bold:true,color:GREEN}},{t:"emissions → concentration → exposure → deaths",o:{bold:true,color:GREEN}},
    "(1) A scenario reports precursor emissions by region and year. (2) TM5-FASST converts them to ambient PM2.5 concentrations. (3) Concentrations are combined with population to give exposure. (4) An exposure–response function converts exposure to premature deaths"],
   [{t:"Atmospheric model",o:{bold:true}},"TM5-FASST via the rfasst package",
    "A reduced-form SOURCE–RECEPTOR model: pre-computed coefficients say how much a tonne emitted in region A raises concentration in region B. This captures cross-boundary transport without running a full chemistry-transport model per scenario"],
   [{t:"Precursors",o:{bold:true}},"SO₂ · NOₓ · black carbon · organic matter · NH₃",
    "BC and OM are primary particles emitted directly. SO₂, NOₓ and NH₃ are gases that form SECONDARY particles in the atmosphere — ammonium sulfate and ammonium nitrate — which is why ammonia matters despite not being a particle itself"],
   [{t:"THE INPUT RULE",o:{bold:true,color:GREEN}},{t:"all five reported DIRECTLY at R10",o:{bold:true,color:GREEN}},
    "A scenario qualifies only if it reports every precursor at regional level. No World-to-R10 population disaggregation, no ammonia filled in from a sidecar, no gap-filling. 422 of 643 classified targets qualify; 150 are REJECTED rather than filled (98 report no precursor, 52 have incomplete grids) and 71 have no direct R10 input at all"],
   [{t:"Why the rule exists",o:{bold:true,color:RED}},{t:"models file emissions differently",o:{color:RED}},
    "Ammonia is 6–12% of PM2.5 mortality in IMAGE, POLES-JRC, AIM and MESSAGEix and 0.15% in REMIND, because REMIND's agricultural emissions live in MAgPIE and never reach Emissions|NH3. Filling that gap would compare accounting conventions; requiring direct reporting removes the asymmetry at source"],
   [{t:"Integration",o:{bold:true}},"trapezoidal across decadal nodes, 2020–2100",
    "Annual deaths at each node, area under the curve between nodes, summed. Reported as MILLION CUMULATIVE DEATHS over the century — an absolute count, not a rate, and not per capita"],
   [{t:"The cost",o:{bold:true}},"smaller arms than the other outcomes","42 vs 37 scenarios at World 1.5°C, against 42 vs 43 for jobs. Mortality is always the binding constraint on sample size, which is why its intervals are the widest"]],
  [2.3,3.0,6.8],{t:"MORTALITY",c:TEAL},
  "SPATIAL: concentrations are computed on TM5-FASST's own 56-region grid and aggregated to R10, so cross-border transport between R10 regions is captured. TEMPORAL: emissions at decadal nodes drive concentrations at those nodes; between-node behaviour is interpolated, and no run in this release required interpolation.",9.5);

section("Part two","Diagnostics","What the database contains, and what it can carry.");

table("The sample","From the database to 610 classified scenarios",
  ["","Count","Note"],
  [["Scenarios carrying the classification variables","1,379","must report the CDR components and renewable capacity"],
   [{t:"Classified, full database (A)",o:{bold:true}},{t:"610",o:{bold:true}},"67 + 67 at 1.5°C · 238 + 238 at 2°C — balanced by construction"],
   ["Classified, SCI-vetted (C)","139","6 + 6 at 1.5°C · 63 + 64 at 2°C"],
   ["Model families · holding both arms",{t:"10 · 7",o:{bold:true}},"one fewer than on the engineered axis — see the axis sensitivity"],
   ["Model × scenario-family clusters","335","the unit resampled by the bootstrap"],
   ["Reaching complete World mortality","363 of 610","and only among scenarios the engineered axis also classified"]],
  [4.8,2.0,5.3],{t:"FUNNEL",c:TEAL},
  "The portfolio rule — top tercile on the focal axis and NOT top tercile on the opposing axis — produces equal arms automatically, because both terciles are the same size on the same sample and the same overlap is removed from each.");

table("Model composition","Who populates which arm — and what the wider axis costs",
  ["Model family","High-CDR","High-RE","Holds both arms?","Change from the engineered axis"],
  [[{t:"REMIND",o:{bold:true}},{t:"1",o:{color:CMT}},{t:"224",o:{bold:true,color:RE}},{t:"barely",o:{color:RED}},"73% of the High-RE arm, up from 71%"],
   [{t:"MESSAGEix",o:{bold:true}},{t:"99",o:{bold:true}},{t:"27",o:{bold:true}},{t:"YES — balanced",o:{color:GREEN,bold:true}},"gains 55 High-CDR scenarios: land-heavy pathways it reports"],
   ["IMAGE","71","2",{t:"barely",o:{color:RED}},"+5 High-CDR"],
   ["POLES-JRC","60","9",{t:"yes",o:{color:GREEN}},"+3 High-CDR"],
   ["GCAM","33","5",{t:"yes",o:{color:GREEN}},"−6 High-CDR"],
   ["AIM/CGE","21","14",{t:"yes",o:{color:GREEN}},"−7 High-CDR, +4 High-RE"],
   ["WITCH","8","16",{t:"yes",o:{color:GREEN}},"unchanged"],
   ["TIAM-ECN","12","0",{t:"no",o:{color:RED}},"−9 High-CDR"],
   [{t:"COFFEE",o:{color:RED}},{t:"0",o:{color:RED,bold:true}},"8",{t:"no — LOST",o:{color:RED,bold:true}},"held both arms on the engineered axis (12 v 8)"],
   [{t:"GEM",o:{color:RED}},{t:"0",o:{color:RED,bold:true}},"0",{t:"no — LOST",o:{color:RED,bold:true}},"25 High-CDR on the engineered axis, none here"]],
  [2.4,1.4,1.4,2.2,4.7],{t:"THE COST",c:GOLD},
  "THIS IS THE PRICE OF THE WIDER AXIS AND IT BELONGS IN THE PAPER. Families holding both arms falls from eight to seven, and REMIND's share of High-RE rises slightly. A broader CDR definition admits scenarios whose engineered removal is small and re-sorts which models populate which arm. The within-model check is what decides whether that matters.",10.5);

table("Model composition","The confound is far worse at 1.5°C than the pooled figure suggests",
  ["Model family","1.5°C CDR","1.5°C RE","2°C CDR","2°C RE","Can it be asked within-model?"],
  [[{t:"REMIND",o:{bold:true}},{t:"1",o:{color:CMT}},{t:"59",o:{bold:true,color:RE}},{t:"0",o:{color:CMT}},{t:"165",o:{bold:true,color:RE}},
    {t:"no — it is essentially the High-RE arm",o:{color:RED}}],
   [{t:"MESSAGEix",o:{bold:true}},{t:"25",o:{bold:true}},{t:"3",o:{bold:true}},{t:"74",o:{bold:true}},{t:"24",o:{bold:true}},
    {t:"YES — the ONLY one at 1.5°C, on 3 RE scenarios",o:{color:GOLD,bold:true}}],
   ["IMAGE","23",{t:"0",o:{color:RED}},"48","2",{t:"2°C only, barely",o:{color:RED}}],
   ["POLES-JRC","4","1","56","8",{t:"2°C only",o:{color:GREEN}}],
   ["GCAM","7","1","26","4",{t:"2°C only",o:{color:GREEN}}],
   ["AIM/CGE","1","3","20","11",{t:"2°C only",o:{color:GREEN}}],
   ["WITCH","6",{t:"0",o:{color:RED}},"2","16",{t:"no",o:{color:RED}}],
   ["TIAM-ECN","0","0","12",{t:"0",o:{color:RED}},{t:"no",o:{color:RED}}],
   ["COFFEE","0","0","0","8",{t:"no",o:{color:RED}}],
   [{t:"REMIND's share of the High-RE arm",o:{bold:true,color:RED}},{t:"88%",o:{bold:true,color:RED}},"",{t:"69%",o:{bold:true}},"",
    {t:"families with ≥3 in BOTH arms: 1 at 1.5°C, 4 at 2°C",o:{bold:true}}]],
  [2.3,1.3,1.3,1.3,1.3,4.6],{t:"1.5°C vs 2°C",c:RED},
  "AT 1.5°C THE ARMS ARE ALMOST PERFECTLY SEGREGATED BY MODEL. REMIND supplies 59 of 67 High-RE scenarios and one of 67 High-CDR; IMAGE (23/0) and WITCH (6/0) are pure High-CDR. So 'High-RE against High-CDR' at high ambition is close to 'REMIND against IMAGE, MESSAGEix, WITCH and GCAM'. The pooled 73% REMIND share understates the problem exactly where the headline results sit. This is very likely substantive rather than clerical — at the most stringent target, frameworks diverge structurally in HOW they get there, and that divergence is what sorts them into arms.",10);

table("Model composition","How many models is each arm really made of?",
  ["","High-CDR arm","High-RE arm","Reading"],
  [[{t:"1.5°C — scenarios",o:{bold:true}},"67","67",""],
   ["  families present","7","5",""],
   ["  largest family",{t:"MESSAGEix 37%",o:{color:GREEN}},{t:"REMIND 88%",o:{color:RED,bold:true}},"one model is nearly the whole arm"],
   [{t:"  effective number of models",o:{bold:true}},{t:"3.6",o:{bold:true,color:GREEN}},{t:"1.3",o:{bold:true,color:RED}},
    {t:"a multi-model ensemble against effectively one model",o:{color:RED}}],
   [{t:"2°C — scenarios",o:{bold:true}},"238","238",""],
   ["  families present","7","8",""],
   ["  largest family",{t:"MESSAGEix 31%",o:{color:GREEN}},{t:"REMIND 69%",o:{color:RED,bold:true}},""],
   [{t:"  effective number of models",o:{bold:true}},{t:"4.7",o:{bold:true,color:GREEN}},{t:"2.0",o:{bold:true,color:RED}},
    {t:"better, and still a 2-to-1 asymmetry in breadth",o:{color:GOLD}}]],
  [3.4,2.6,2.6,3.5],{t:"CONCENTRATION",c:RED},
  "Effective number of models = 1 / Σ(share²), the inverse Herfindahl index: it asks how many models an arm is really made of, so an arm that is 88% one model counts as ~1 however long its tail. THE ASYMMETRY IS THE PROBLEM, not the overlap between arms. High-CDR is a genuine ensemble at both ambition levels; High-RE is REMIND with a fringe. A pooled difference between them could be a pathway effect or a REMIND effect, and nothing in the composition alone distinguishes the two — which is what the next slide tests.",10);

table("Diagnostics","World coverage flow — how many scenarios each outcome can actually use",
  ["","1.5°C CDR","1.5°C RE","2°C CDR","2°C RE","What gates it"],
  [[{t:"Classified",o:{bold:true}},{t:"67",o:{bold:true}},{t:"67",o:{bold:true}},{t:"238",o:{bold:true}},{t:"238",o:{bold:true}},
    "Top tercile on the focal axis, not top tercile on the opposing axis"],
   [{t:"Complete World jobs",o:{bold:true}},"58","67","212","234",
    "Requires renewable AND fossil jobs in all ten R10 regions"],
   [{t:"Complete World deprivation",o:{bold:true}},"67","67","223","226",
    "Requires the decent-living gap in all ten regions"],
   [{t:"Complete World mortality",o:{bold:true}},{t:"37",o:{color:RED}},{t:"37",o:{color:RED}},{t:"133",o:{color:RED}},{t:"156",o:{color:RED}},
    "All five PM2.5 precursors reported directly at R10 — the strictest gate"],
   [{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}}],
   [{t:"Retention, jobs",o:{color:MUTE}},{t:"87%",o:{color:MUTE}},{t:"100%",o:{color:MUTE}},{t:"89%",o:{color:MUTE}},{t:"98%",o:{color:MUTE}},
    {t:"The three outcomes are gated INDEPENDENTLY — missing mortality does not blank jobs",o:{color:MUTE}}],
   [{t:"Retention, mortality",o:{color:MUTE}},{t:"55%",o:{color:MUTE}},{t:"55%",o:{color:MUTE}},{t:"56%",o:{color:MUTE}},{t:"66%",o:{color:MUTE}},
    {t:"Mortality is always the binding constraint on sample size",o:{color:MUTE}}]],
  [3.2,1.5,1.5,1.5,1.5,3.0],{t:"COVERAGE",c:TEAL},
  "A World figure is computed ONLY when all ten R10 regions are present for that outcome; partial sums are set to NA rather than reported. MORTALITY CARRIES AN EXTRA CAVEAT: its target list was drawn against the engineered axis, so the 80 scenarios the all-CDR axis newly admits have no mortality run at all. That is target selection, not a property of those scenarios, and it is why the mortality arms are the smallest here.",10);

img("Diagnostics","The same coverage flow, drawn",FIG+"F4_coverage.png",
  "Each outcome is gated on its own ten-region completeness, so the arms differ in size between outcomes.",
  "Deprivation retains MORE High-CDR scenarios than jobs at 1.5°C (67 against 58), which is exactly why the three outcomes have to be gated separately rather than jointly. Mortality is the binding constraint everywhere — and here it is doubly so, because its targets were drawn against the engineered axis.",
  {t:"FIGURE F4",c:TEAL},0.5);

bullets("Diagnostics","Two data defects found and fixed, and what each one moved",
  ["DEFECT ONE — THE WORLD AGGREGATION. The master built the World row by summing outcomes WITHIN each deployment-variable group, so a scenario's World total inherited the regional coverage of whichever CDR variable that row belonged to. COFFEE 1.1 / COMMIT-Baseline read 765,457 on one row and 684,824 on the other; all 288 discrepant scenario-regions showed exactly this pattern.",
   "FIXED AND PORTED. World outcomes are now built from an outcome-only R10 table, each gated INDEPENDENTLY on having all ten regions, with explicit coverage fields. Ported into build_df_master() and verified to machine precision.",
   "DEFECT TWO — MANGLED SCENARIO KEYS, AND LARGER. The labels file stores degree signs as literal text where the master files carry a real degree sign, so the two spellings never compare equal. The published summary joins them with inner_join(), which drops non-matching rows SILENTLY — 71 classified scenarios (REMIND 34, IMAGE 20, COFFEE 6, WITCH 6, GCAM 3, MESSAGEix 2) never reached the outcome tables.",
   "WHAT REPAIRING THE KEYS MOVED. On the engineered axis, World 1.5°C jobs goes from +395.8 to +405.0 and its arms from 42 v 43 to 53 v 64; the scorecard from 42 of 60 to 44 of 60. No direction changes and no cell reverses — but the published sample was 12% smaller than it should have been, and every number here is now on repaired keys."],
  {t:"TWO FIXES",c:GREEN},
  "Defect one: V3_world_strict.R, ported and verified by V4_verify_port.R. Defect two: diagnosed in V6_key_repair.R, which shows all 746 labels joining after repair against 675 before. Both are upstream defects in the published pipeline, not artefacts of these scripts.");

section("Part three","The wellbeing results","What High-RE delivers, at World and in every region.");

img("Result — the whole grid","All 60 comparisons on one panel",FIG+"F1_scorecard.png",
  "Every region × ambition × outcome cell, coloured by direction, with the saturated fill marking the cells whose interval clears zero.",
  "The jobs column is solid: twenty of twenty, all significant. Deprivation clears in eleven cells and mortality in five, with reversals in both — the unevenness is the paper's actual finding rather than a weakness in it. Pacific OECD is excluded from the regional rows and retained inside the World aggregate.",
  {t:"FIGURE F1",c:GOLD},0.571);

function regtable(title,tag,unitnote,H,R,note){
  const s2=slide(); head(s2,"Result — regions",title,tag);
  s2.addText("1.5°C HIGH AMBITION",{x:2.72,y:1.50,w:4.9,h:0.24,fontSize:9.5,bold:true,
    color:TEAL,fontFace:F,charSpacing:1.3,align:"center"});
  s2.addText("2°C MEDIUM AMBITION",{x:7.75,y:1.50,w:4.9,h:0.24,fontSize:9.5,bold:true,
    color:TEAL,fontFace:F,charSpacing:1.3,align:"center"});
  const body=[H.map(h=>({text:h,options:{bold:true,color:MUTE,fontSize:9,fill:{color:PANEL},fontFace:F}}))];
  R.forEach(r=>body.push(r.map(c=>{const o={fontSize:10.5,fontFace:F,color:INK,valign:"top"};
    if(typeof c==="object"){Object.assign(o,c.o||{});return {text:c.t,options:o};} return {text:c,options:o};})));
  s2.addTable(body,{x:0.62,y:1.78,w:12.1,colW:[2.5,1.6,1.6,1.75,1.6,1.6,1.75],
    border:{type:"solid",color:LINE,pt:0.5},rowH:0.39,autoPage:false});
  s2.addText(note,{x:0.62,y:6.55,w:12.1,h:0.7,fontSize:10,color:MUTE,fontFace:F,lineSpacing:14});
}

regtable("Energy jobs: job-years per 1,000 people, 2020–2100",{t:"20 / 20",c:GOLD},"",
  ["Region","High-CDR","High-RE","Difference","High-CDR","High-RE","Difference"],
  [
   ["WORLD","227.60","691.52",{t:"+463.9 ●",o:{color:GREEN,bold:true}},"186.50","471.15",{t:"+284.6 ●",o:{color:GREEN,bold:true}}],
   ["Africa","263.10","1043.61",{t:"+780.5 ●",o:{color:GREEN,bold:true}},"229.18","723.48",{t:"+494.3 ●",o:{color:GREEN,bold:true}}],
   ["China+","177.25","372.99",{t:"+195.7 ●",o:{color:GREEN}},"129.51","266.25",{t:"+136.7 ●",o:{color:GREEN}}],
   ["Europe","65.91","171.10",{t:"+105.2 ●",o:{color:GREEN}},"60.08","116.37",{t:"+56.3 ●",o:{color:GREEN}}],
   ["India+","264.43","1007.64",{t:"+743.2 ●",o:{color:GREEN,bold:true}},"242.52","668.16",{t:"+425.6 ●",o:{color:GREEN,bold:true}}],
   ["Latin America","232.43","508.68",{t:"+276.2 ●",o:{color:GREEN}},"178.72","372.57",{t:"+193.8 ●",o:{color:GREEN}}],
   ["Middle East","339.12","851.03",{t:"+511.9 ●",o:{color:GREEN,bold:true}},"153.83","549.95",{t:"+396.1 ●",o:{color:GREEN,bold:true}}],
   ["North America","140.29","328.29",{t:"+188.0 ●",o:{color:GREEN}},"101.32","219.54",{t:"+118.2 ●",o:{color:GREEN}}],
   ["Reforming econ.","503.39","764.10",{t:"+260.7 ●",o:{color:GREEN}},"383.21","605.37",{t:"+222.2 ●",o:{color:GREEN}}],
   ["Rest of Asia","155.22","874.24",{t:"+719.0 ●",o:{color:GREEN,bold:true}},"129.18","543.04",{t:"+413.9 ●",o:{color:GREEN,bold:true}}],],
  "● clears a cluster-robust 95% interval on the raw difference. EVERY cell favours High-RE and EVERY cell clears the interval — the only outcome in the study of which that is true. Pacific OECD is excluded from these rows (+37.6 at 1.5°C, +22.6 at 2°C, neither significant) and retained in the World aggregate.");

regtable("Energy deprivation: decent-living gap, GJ per capita",{t:"14 / 20",c:GOLD},"",
  ["Region","High-CDR","High-RE","Gap closed","High-CDR","High-RE","Gap closed"],
  [
   ["WORLD","18.43","11.62",{t:"-6.81 ●",o:{color:GREEN,bold:true}},"15.46","11.37",{t:"-4.09",o:{color:MUTE}}],
   ["Africa","67.27","42.51",{t:"-24.76 ●",o:{color:GREEN,bold:true}},"64.88","41.84",{t:"-23.04",o:{color:MUTE}}],
   ["China+","1.20","0.80",{t:"-0.40 ●",o:{color:GREEN}},"0.83","0.67",{t:"-0.16 ●",o:{color:GREEN}}],
   ["Europe","6.21","2.65",{t:"-3.56 ●",o:{color:GREEN,bold:true}},"3.46","2.64",{t:"-0.82 ●",o:{color:GREEN,bold:true}}],
   ["India+","1.35","0.47",{t:"-0.88 ●",o:{color:GREEN}},"1.08","0.44",{t:"-0.64 ●",o:{color:GREEN}}],
   ["Latin America","61.80","37.38",{t:"-24.42 ●",o:{color:GREEN,bold:true}},"51.20","38.97",{t:"-12.22 ●",o:{color:GREEN,bold:true}}],
   ["Middle East","1.83","2.43",{t:"+0.60",o:{color:MUTE}},"0.94","2.24",{t:"+1.31 ●",o:{color:RED,bold:true}}],
   ["North America","10.26","7.62",{t:"-2.64 ●",o:{color:GREEN}},"7.19","8.06",{t:"+0.88",o:{color:MUTE}}],
   ["Reforming econ.","2.00","1.55",{t:"-0.45",o:{color:MUTE}},"1.37","1.42",{t:"+0.06",o:{color:MUTE}}],
   ["Rest of Asia","4.28","8.06",{t:"+3.78",o:{color:MUTE}},"3.62","7.06",{t:"+3.44 ●",o:{color:RED,bold:true}}],],
  "Negative closes the gap and favours High-RE. Eleven of twenty cells clear, up from six on the engineered axis. Africa and Latin America carry the largest absolute movements; Latin America clears at both levels and Africa only at 1.5°C. Rest of Asia reverses significantly at 2°C and the Middle East at 2°C.");

regtable("PM2.5 mortality: million cumulative deaths, 2020–2100",{t:"8 / 20",c:GOLD},"",
  ["Region","High-CDR","High-RE","Deaths avoided","High-CDR","High-RE","Deaths avoided"],
  [
   ["WORLD","426.23","415.12",{t:"+11.11 ●",o:{color:GREEN,bold:true}},"433.60","437.88",{t:"-4.28",o:{color:MUTE}}],
   ["Africa","75.29","78.51",{t:"-3.22 ●",o:{color:RED}},"76.28","78.70",{t:"-2.41 ●",o:{color:RED}}],
   ["China+","79.07","77.37",{t:"+1.70",o:{color:MUTE}},"79.58","78.84",{t:"+0.74",o:{color:MUTE}}],
   ["Europe","16.15","11.16",{t:"+4.99 ●",o:{color:GREEN,bold:true}},"14.89","11.23",{t:"+3.66 ●",o:{color:GREEN,bold:true}}],
   ["India+","112.91","104.60",{t:"+8.30",o:{color:MUTE}},"114.35","116.34",{t:"-1.98",o:{color:MUTE}}],
   ["Latin America","15.19","15.66",{t:"-0.47",o:{color:MUTE}},"13.75","15.65",{t:"-1.89",o:{color:MUTE}}],
   ["Middle East","46.15","46.24",{t:"-0.09",o:{color:MUTE}},"43.78","46.61",{t:"-2.83",o:{color:MUTE}}],
   ["North America","7.47","8.92",{t:"-1.45",o:{color:MUTE}},"6.79","8.93",{t:"-2.13",o:{color:MUTE}}],
   ["Reforming econ.","13.97","13.08",{t:"+0.88 ●",o:{color:GREEN}},"14.14","13.25",{t:"+0.89 ●",o:{color:GREEN}}],
   ["Rest of Asia","55.38","56.11",{t:"-0.74",o:{color:MUTE}},"57.89","57.92",{t:"-0.04",o:{color:MUTE}}],],
  "Positive means High-RE avoids deaths. EUROPE CARRIES THE WORLD RESULT: 4.99 and 3.66 million avoided, significant at both levels, on a base of about 16 million — a 31% reduction, the largest proportional effect anywhere. Africa goes significantly the other way at both levels. Twelve of twenty cells straddle zero. Pacific OECD (+1.70, +1.91, both significant) is excluded from these rows.");

img("Result — World","The three World cells, with their intervals",FIG+"F2_world.png",
  "Jobs clears by a wide margin at both ambition levels; deprivation and mortality clear at 1.5°C only.",
  "Each bar is the raw difference in medians with a 2,000-replicate cluster bootstrap over 290 model × scenario-family clusters. Four of the six World cells clear, and all four favour High-RE.",
  {t:"FIGURE F2",c:GOLD},0.417);

table("Result — the scorecard","Every design, side by side",
  ["Design","Cells","POINT to High-RE","Point to High-CDR","Of which clear the interval"],
  [[{t:"Full database · 1.5°C",o:{bold:true}},"30",{t:"23",o:{bold:true,color:GREEN}},"7","20 · 1"],
   [{t:"Full database · 2°C",o:{bold:true}},"30",{t:"19",o:{bold:true,color:GREEN}},"11","16 · 3"],
   ["SCI-vetted · 1.5°C","30",{t:"24",o:{bold:true}},"6","13 · 0"],
   ["SCI-vetted · 2°C","30","17","13","12 · 3"],
   [{t:"BY OUTCOME — full database, both levels",o:{bold:true,color:TEAL}},"","","",""],
   [{t:"  Energy jobs",o:{bold:true}},"20",{t:"20",o:{bold:true,color:GREEN}},{t:"0",o:{color:GREEN}},"20 · 0"],
   ["  Energy deprivation","20",{t:"14",o:{color:GOLD}},"6","11 · 2"],
   ["  PM2.5 mortality","20",{t:"8",o:{color:RED}},{t:"12",o:{color:RED}},"5 · 2"],
   [{t:"  TOTAL",o:{bold:true}},{t:"60",o:{bold:true}},{t:"42",o:{bold:true,color:GREEN}},{t:"18",o:{bold:true}},"36 · 4"],
   [{t:"ENGINEERED-ONLY AXIS (the sensitivity)",o:{bold:true,color:MUTE}},{t:"60",o:{color:MUTE}},{t:"44",o:{color:MUTE}},{t:"16",o:{color:MUTE}},{t:"32 · 3",o:{color:MUTE}}]],
  [4.2,1.4,2.2,2.2,2.1],{t:"42 / 60",c:GOLD},
  "DIRECTION FIRST. The last column is 'for · against' among the cells whose interval clears zero, kept as context rather than as the claim. Read by direction, the three outcomes are plainly different: jobs points one way everywhere, deprivation leans High-RE with six exceptions, and mortality is close to a coin flip at 8 against 12. The axis choice barely moves any of it — the engineered-only sensitivity gives 44 of 60. The SCI-vetted rows run on six scenarios per arm at 1.5°C and score highest because they are smallest; their labels also differ from the published ones by seven scenarios, so treat them as indicative.");

section("Part four","Why — mechanism and real world","Why these results, given how the outcomes are built and how energy systems work.");

table("Mechanism — jobs","Where the employment gap comes from, and what High-CDR wins",
  ["Technology group","High-CDR","High-RE","Gap","Reading"],
  [[{t:"Renewables",o:{bold:true}},"252.3","703.7",{t:"+451.4",o:{bold:true,color:GREEN}},
    "The entire result — wind and solar construction, manufacturing and O&M"],
   [{t:"Fossil",o:{bold:true}},"33.6","11.3",{t:"−22.3",o:{bold:true,color:CMT}},
    "The largest single loss. Fossil CCS extends the fuel supply chain — the mine and the well keep running"],
   ["Nuclear","26.2","16.1",{t:"−10.1",o:{color:CMT}},
    "High-CDR leans on firm low-carbon capacity that High-RE displaces"],
   ["Bioenergy","16.4","6.5",{t:"−9.9",o:{color:CMT}},
    "BECCS and land-based removal carry a biomass supply workforce"],
   [{t:"NET",o:{bold:true}},{t:"227.60",o:{bold:true}},{t:"691.52",o:{bold:true}},{t:"+463.9",o:{bold:true,color:GREEN}},
    {t:"Renewables outweigh all three losses combined by 11 to 1",o:{bold:true}}]],
  [2.4,1.5,1.5,1.5,5.2],{t:"1.5°C, WORLD",c:TEAL},
  "Job-years per 1,000 people, cumulative 2020–2100, medians over the scenarios with complete ten-region coverage. THE MECHANISM IS UNAMBIGUOUS: High-CDR wins fossil, nuclear and bioenergy employment combined (−42.3) and still loses by 464, because the renewable build is 2.8 times larger. This also answers the obvious objection — the result is NOT an artefact of counting fossil job destruction, because High-CDR retains more fossil, nuclear AND biomass work and loses anyway.");

bullets("Why — World","The jobs mechanism is an engineering fact, not a modelling artefact",
  ["LABOUR INTENSITY PER UNIT OF CAPACITY IS HIGHER FOR BUILDING THAN FOR FUELLING. A gas turbine with capture needs a fuel supply chain and a handful of operators; an equivalent wind or solar build needs factories, foundations, cabling and installation crews. Over 2020–2100 the renewable term is 704 job-years per 1,000 against 252 — 2.8 times larger — and that single term carries the entire result.",
   "CARBON REMOVAL IS CAPITAL-INTENSIVE AND LABOUR-SPARING BY DESIGN. Its appeal is that it lets the existing energy system keep running with a bolt-on. That is exactly why it employs fewer people: the point of the technology is to avoid rebuilding the system, and rebuilding the system is where the work is. Land-based removal is more labour-sparing still in ENERGY terms — its work is in land management, which this outcome set does not count.",
   "THE THREE THINGS HIGH-CDR WINS ARE REAL AND WORTH NAMING. Fossil (+22), nuclear (+10) and bioenergy (+10) all favour it. These are not trivial jobs — fossil extraction is regionally concentrated, nuclear is high-skill and unionised, biomass supply is rural. A just-transition argument has to engage with all three, and the paper should say so rather than reporting only the net.",
   "THE REAL-WORLD EVIDENCE POINTS THE SAME WAY. Observed employment per GW installed in solar PV and onshore wind exceeds that of new thermal capacity in essentially every published national accounting, and the gap is widest in the build phase. The models are reproducing something already visible in labour statistics."],
  {t:"WORLD",c:GOLD});

bullets("Why — World","Why the deprivation result now clears at high ambition",
  ["THE MECHANISM IS DELIVERED FINAL ENERGY. The gap is max(0, threshold − actual) applied per sector and truncated at zero, so a pathway closes it by delivering more usable energy to the sectors that fall short. High-RE closes the World gap from 18.43 to 11.62 GJ per capita at 1.5°C — a 37% reduction, and the interval clears zero at [+1.72, +9.73].",
   "WHY IT CLEARS NOW AND DID NOT BEFORE. Two things changed and both added scenarios rather than changing any value. Repairing the mangled scenario keys returned 71 classified scenarios that a text-encoding mismatch had silently dropped, and the all-CDR axis admits 80 more. Deprivation is the outcome with the widest between-model spread, so it is the one most starved by a smaller sample — the point estimate barely moved; the interval tightened.",
   "IT IS STILL A REGIONALLY SPLIT RESULT. Latin America (−24.4), Africa (−24.8), Europe (−3.6), North America (−2.6), India+ (−0.9) and China+ (−0.4) all clear at 1.5°C. Rest of Asia (+3.8 at 1.5°C, +3.4 at 2°C) and the Middle East (+1.3 at 2°C) push the other way, the latter two significantly. That is a genuine split, not a weak global effect.",
   "AND IT DOES NOT HOLD AT 2°C. The World cell is −4.09 [−0.56, +7.75] — large, correctly signed, and not significant. Report deprivation as holding at high ambition and as regionally split at medium ambition; that is more honest and more useful than a single global claim."],
  {t:"WORLD",c:GOLD});

bullets("Why — World","Why mortality clears at 1.5°C, and why Europe carries it",
  ["THE INPUT RULE IS WHAT MADE MORTALITY REPORTABLE AT ALL. Requiring all five PM2.5 precursors to be reported directly at R10 removes the ammonia asymmetry at source. In the previous round REMIND reported almost no agricultural ammonia while other models reported 6–12%, flattering High-RE by roughly 9%. Scenarios that cannot support the calculation are now dropped rather than filled.",
   "EUROPE IS THE RESULT. 4.99 million cumulative deaths avoided at 1.5°C and 3.66 million at 2°C, both significant, on a base of about 16 million — a 31% reduction, the largest proportional mortality effect of any region. Europe's population lives densely and close to its combustion sources, so a given reduction in primary PM and precursors buys more avoided exposure there than almost anywhere.",
   "THE WORLD RESULT IS SIGNIFICANT AT 1.5°C AND NOT AT 2°C — 11.11 million avoided [+3.82, +50.40] against −4.28 [−20.6, +35.9]. That asymmetry is consistent with the mechanism: at higher ambition the two arms differ more in how much combustion remains, so the air-quality consequence is larger.",
   "AND AFRICA GOES THE OTHER WAY, SIGNIFICANTLY, AT BOTH LEVELS — 3.2 and 2.4 million more deaths under High-RE. This is the study's most uncomfortable cell and it should be reported prominently. African PM2.5 is dominated by residential solid fuel and windblown dust, which the power-sector mix does not control; a pathway that builds generation without displacing household solid fuel does not buy clean air there. Twelve of twenty cells still straddle zero, so there is no universal air-quality co-benefit to claim."],
  {t:"WORLD",c:GOLD});

section("Part five","Why — region by region","Each region's own mechanism, and its real-world reading.");

function R2(name,verdict,vc,jobs,dep,hea,extra,mech,real,note){
  const st=[["Jobs 1.5°C / 2°C",jobs[0],jobs[1]],
            ["Deprivation gap, GJ/cap",dep[0],dep[1]],
            ["Mortality, million deaths",hea[0],hea[1]]].concat(extra);
  region(name,verdict,vc,st,mech,real,note);
}

R2("India+ — the largest employment prize in the study","3 / 3",GREEN,
  ["264.4 → 1007.6 · 242.5 → 668.2",GREEN],["1.35 → 0.47 · 1.08 → 0.44",GREEN],
  ["112.9 v 104.6 · 114.3 v 116.3",null],
  [["Jobs multiple","3.8× · 2.8×",GREEN],["Deprivation, both levels","significant",GREEN],
   ["Baseline PM2.5 burden","113–116 m deaths",RED],["Mortality 1.5°C","+8.3 m, not significant"]],
  "India+ shows the second-largest absolute jobs gap of any region (+743 job-years per 1,000 at 1.5°C) on top of an already-high High-CDR base — the renewable build is 3.8 times larger, not merely additive. Its decent-living threshold is the lowest anywhere at 10 GJ/cap, so the small absolute deprivation gap (0.47 GJ/cap under High-RE) still clears the interval at BOTH ambition levels, which India+, China+, Europe and Latin America all now manage.",
  "India's power build is genuinely greenfield: there is no large stranded workforce to displace and the solar manufacturing base is being built now rather than defended. The carbon-removal alternative here means coal or gas with capture, which locks in both the mine and the import bill. India+ also carries the largest absolute PM2.5 burden of any region at 113–116 million cumulative deaths, so even a non-significant 8.3 million avoided at 1.5°C is a large number in human terms.",
  "The mortality cells do not clear at either level and the sign flips between them, so India+ supports the jobs and deprivation claims but not an air-quality claim.");

R2("Europe — the only region where all three outcomes hold","3 / 3",GREEN,
  ["65.9 → 171.1 · 60.1 → 116.4",GREEN],["6.21 → 2.65 · 3.46 → 2.64",GREEN],
  ["16.2 v 11.2 · 14.9 v 11.2",GREEN],
  [["Mortality reduction","31% · 25%",GREEN],["All three outcomes","significant at 1.5°C",GREEN],
   ["Jobs, smallest absolute gap","+105 · +56"],["Deprivation, both levels","significant",GREEN]],
  "Europe has the SMALLEST absolute jobs gap of any region (+105 job-years per 1,000) because its build is small relative to its incumbent system — and it still clears the interval at both levels. Its distinguishing result is mortality: 4.99 and 3.66 million cumulative deaths avoided on a base of about 16 million, a 31% and 25% reduction. That is the largest proportional air-quality effect anywhere and the only mortality result significant at both ambition levels.",
  "Europe's population lives densely and close to its combustion sources, so a given reduction in primary PM and precursors buys more avoided exposure than almost anywhere else. It is also the region where the reporting-complete rule bites least — European models report the full precursor set — so the mortality estimate rests on comparatively solid input data. Europe is the region to lead with when the audience is air-quality policy.",
  "Europe is the ONLY region where all three outcomes clear at 1.5°C, and it is the natural anchor for the paper's three-outcome claim.");

R2("Africa — the largest jobs gap, and the study's hardest cell","2 / 3",GOLD,
  ["263.1 → 1043.6 · 229.2 → 723.5",GREEN],["67.3 → 42.5 · 64.9 → 41.8",GREEN],
  ["75.3 v 78.5 · 76.3 v 78.7",RED],
  [["Jobs multiple","4.0× · 3.2×",GREEN],["Deprivation 1.5°C","−24.8, significant",GREEN],
   ["Baseline deprivation gap","65–67 GJ/cap",RED],["Mortality, BOTH levels","significantly AGAINST",RED]],
  "Africa has the largest jobs gap in the study by a wide margin: +781 job-years per 1,000 at 1.5°C, because there is almost nothing to retire and everything to build. Its deprivation baseline is also by far the worst — a 65–67 GJ/cap shortfall against a 17 GJ/cap threshold — and High-RE closes 23–25 GJ/cap of it, which NOW CLEARS at 1.5°C on the larger sample, though not at 2°C. But mortality goes significantly the other way at BOTH ambition levels: 3.2 and 2.4 million more deaths.",
  "Africa is where the wellbeing stakes are highest and the three outcomes disagree most sharply. The mortality reversal is the important finding and it has a clear mechanism: African PM2.5 is dominated by residential solid fuel and windblown dust, not the power mix, so a pathway that builds generation without displacing household cooking fuel does not buy clean air. It may even shift exposure, because the build itself is dusty and the displaced capacity was never the binding source.",
  "The paper should report Africa's mortality cell prominently rather than treating it as noise. It is the clearest evidence that the air-quality co-benefit is not universal and depends on what actually dominates local exposure.");

R2("Latin America — the most complete deprivation result","2 / 3",GREEN,
  ["232.4 → 508.7 · 178.7 → 372.6",GREEN],["61.8 → 37.4 · 51.2 → 39.0",GREEN],
  ["15.2 v 15.7 · 13.8 v 15.7",null],
  [["Deprivation, both levels","significant",GREEN],["Gap closed 1.5°C","−24.4 GJ/cap",GREEN],
   ["Baseline deprivation gap","51–62 GJ/cap",RED],["Land CDR","now INSIDE the axis",TEAL]],
  "Latin America closes the largest deprivation gap that clears at both ambition levels: −24.4 GJ/cap at 1.5°C and −12.2 at 2°C. Its models agree, so the result is both large and precise. Jobs is solid (+276 and +194) though not among the largest multiples, because Latin America already has a substantial renewable base and the incremental build is smaller in relative terms.",
  "This region is the one most affected by the axis decision. Under an engineered-only axis, Latin American carbon management is largely INVISIBLE — its removal is overwhelmingly land-based, afforestation and soil carbon — so an engineered axis scores the region on an intervention it barely uses. Putting land back in makes Latin America a genuine High-CDR region for the first time, which is the single strongest argument for the all-CDR axis being the right primary specification.",
  "Mortality is mildly negative at both levels and clears at neither. Latin America's PM2.5 base is small (14–16 million) so there is little room for the power mix to move it.");

R2("Middle East — jobs yes, energy access no","1 / 3",GOLD,
  ["339.1 → 851.0 · 153.8 → 550.0",GREEN],["1.83 → 2.43 · 0.94 → 2.24",RED],
  ["46.2 v 46.2 · 43.8 v 46.6",null],
  [["Jobs multiple","2.5× · 3.6×",GREEN],["Deprivation 2°C","significantly AGAINST",RED],
   ["Deprivation 1.5°C","−0.60 against, not significant"],["Baseline PM2.5","44–47 m deaths"]],
  "The Middle East has one of the largest jobs multiples in the study (3.6× at 2°C) and is the region where deprivation most clearly reverses — the gap WIDENS from 0.94 to 2.24 GJ/cap at 2°C, significantly. Those two facts sit together because the region starts from an unusually low renewable share and an unusually high per-capita energy base: building renewables creates a great deal of work while the delivered-energy trajectory under High-RE falls short of what the carbon-removal pathways sustain.",
  "For the Gulf, carbon management is not a climate policy bolted onto the economy — it IS the economy. Fossil CCS lets hydrocarbon production and export continue while the emissions are captured. High-RE means abandoning the export base, and the models reflect the domestic energy consequence of doing so. This is the clearest case in the study where the wellbeing comparison and the political-economy reality point in genuinely different directions.",
  "On the wider CDR axis the 1.5°C deprivation cell no longer clears (−0.60, not significant) where it did on the engineered axis, so the reversal is now a 2°C finding rather than a universal one.");

R2("China+ — solid on both, quiet on health","2 / 3",GREEN,
  ["177.3 → 373.0 · 129.5 → 266.3",GREEN],["1.20 → 0.80 · 0.83 → 0.67",GREEN],
  ["79.1 v 77.4 · 79.6 v 78.8",null],
  [["Jobs multiple","2.1× · 2.1×",null],["Deprivation, both levels","significant",GREEN],
   ["Smallest jobs multiple","2.1×"],["Baseline PM2.5","79–80 m deaths",RED]],
  "China+ shows the SMALLEST jobs multiple of any region at 2.1× — but on a large base, so the absolute gap (+196 and +137 job-years per 1,000) still clears the interval comfortably at both levels. The modest multiple reflects that China's High-CDR pathways already involve a very large build; the two arms differ less in whether they construct than in what they construct. Deprivation now clears at BOTH ambition levels on the larger sample.",
  "China+ carries the second-largest absolute PM2.5 burden in the study (79–80 million cumulative deaths) and the mortality difference between arms is small and not significant in either direction. The plausible reason is that Chinese PM2.5 is heavily influenced by agricultural ammonium nitrate and industrial process emissions that neither pathway addresses — the power-sector mix is not the binding constraint on Chinese air quality within this horizon.",
  "China+ is a reliable contributor to the headline without being the story anywhere: two of three outcomes clear at both levels, and the third is a genuine null.");

R2("Rest of Asia — the largest multiple, and a deprivation reversal","1 / 3",GOLD,
  ["155.2 → 874.2 · 129.2 → 543.0",GREEN],["4.28 → 8.06 · 3.62 → 7.06",RED],
  ["55.4 v 56.1 · 57.9 v 57.9",null],
  [["Jobs multiple","5.6× · 4.2×",GREEN],["Largest multiple in the study","5.6× at 1.5°C",GREEN],
   ["Deprivation 2°C","+3.44, significantly against",RED],["Deprivation 1.5°C","+3.78, not significant",RED]],
  "Rest of Asia has the LARGEST jobs multiple anywhere — 5.6× at 1.5°C, from 155 to 874 job-years per 1,000 — for the same reason as Africa and India+: a very large remaining build against a small incumbent fleet. Deprivation goes the other way at both levels and clears at 2°C, making it one of only two regions where High-RE significantly WIDENS the decent-living gap.",
  "The region aggregates economies at very different stages — Indonesia, Vietnam, Thailand, the Philippines — so a single R10 label conceals more heterogeneity here than almost anywhere. The deprivation reversal is most plausibly a composition effect within the region rather than a coherent mechanism, and the paper should say so rather than construct a story for it.",
  "The jobs result is among the strongest in the study; the deprivation result is a genuine reversal at 2°C and should be reported as such.");

R2("North America — jobs and access hold at high ambition","2 / 3",GREEN,
  ["140.3 → 328.3 · 101.3 → 219.5",GREEN],["10.26 → 7.62 · 7.19 → 8.06",null],
  ["7.5 v 8.9 · 6.8 v 8.9",RED],
  [["Jobs multiple","2.3× · 2.2×",GREEN],["Deprivation 1.5°C","−2.64, significant",GREEN],
   ["Deprivation 2°C","+0.88, not significant"],["Smallest PM2.5 base","6.8–8.9 m deaths"]],
  "North America's jobs gap is solid and significant at both levels (+188 and +118) on a 2.2–2.3× multiple. Deprivation now CLEARS at 1.5°C (−2.64 GJ/cap) where on the engineered axis it went mildly the other way — one of the cells the wider axis and the repaired sample changed. At 2°C it reverts to a small non-significant widening. Mortality goes mildly against High-RE at both levels and clears at neither.",
  "North America has the highest incumbent nuclear capacity of any region, and the jobs decomposition shows nuclear employment favouring High-CDR globally by 10 job-years per 1,000 — a penalty concentrated in exactly this region. The deprivation reading needs care: at 37 GJ/cap North America has the highest decent-living threshold anywhere, so its residual gap measures how far a rich region sits from a demanding benchmark rather than energy poverty in the usual sense.",
  "The mortality base is the smallest of any region at 6.8–8.9 million, so there is very little room for the power mix to move it — the non-significant difference is what one would expect rather than a puzzle.");

R2("Reforming economies — strong jobs, and a rare health signal","2 / 3",GREEN,
  ["503.4 → 764.1 · 383.2 → 605.4",GREEN],["2.00 → 1.55 · 1.37 → 1.42",null],
  ["14.0 v 13.1 · 14.1 v 13.3",GREEN],
  [["Highest jobs base anywhere","503 · 383"],["Jobs, both levels","significant",GREEN],
   ["Mortality, BOTH levels","significant",GREEN],["Deprivation","flat, not significant"]],
  "Reforming economies carries the highest High-CDR jobs base of any region (503 job-years per 1,000 at 1.5°C) — a reflection of its very large incumbent energy workforce — and High-RE still adds +261 on top of it. It is also one of only three regions with a significant mortality result favouring High-RE, and on the all-CDR axis it now clears at BOTH ambition levels (+0.88 and +0.89 million avoided), alongside Europe and Pacific OECD.",
  "Russia and Central Asia hold the largest fossil fleet in the study, so this is the region where a transition looks most like a net industrial disruption. That the jobs result still clears comfortably — and that it does so while High-CDR retains more fossil, nuclear and biomass employment — is the strongest available evidence that the renewable build genuinely outweighs what it displaces rather than merely accounting it away.",
  "Deprivation is flat (−0.45 and +0.06, neither significant), which is what one would expect of a region already close to its decent-living threshold.");

R2("Pacific OECD — excluded from the regional rows","EXCLUDED",RED,
  ["81.7 → 119.3 · 75.2 → 97.8",null],["5.53 → 5.93 · 3.66 → 5.38",RED],
  ["5.3 v 3.6 · 5.5 v 3.6",GREEN],
  [["Jobs 1.5°C","+37.6, not significant",RED],["Jobs 2°C","+22.6, not significant",RED],
   ["Deprivation 2°C","+1.73, significant against",RED],["Mortality","+1.7 · +1.9, both significant",GREEN]],
  "Pacific OECD is the only region where the jobs result fails at BOTH ambition levels — the gap is positive but the interval never clears. It is also the only region whose mortality cells are significant while its jobs cells are not, which is the reverse of the pattern everywhere else. Deprivation goes significantly against High-RE at 2°C.",
  "The region is small (Japan, Korea, Australia, New Zealand), highly developed, and already substantially built out, so the marginal renewable build that distinguishes the two arms elsewhere is much smaller here. As in the previous design, the classification does not describe local behaviour well enough to support a regional reading.",
  "Retained inside the World aggregate — dropping it there would change what 'World' means — and excluded from the nine regional rows. This is a display decision, not a data exclusion: nothing is discarded.");

section("Part six","But is it real?",
  "Model composition, the land-based CDR boundary, and the scenarios that never reported renewables.");

bullets("The confound","An artefact of the database, and the binding constraint on attribution",
  ["ONE FAMILY DOMINATES ONE ARM, AND IT IS WORST WHERE THE HEADLINES ARE. REMIND is 73% of High-RE pooled — 88% at 1.5°C and 69% at 2°C — against under 1% of High-CDR. At high ambition IMAGE (23/0) and WITCH (6/0) are pure High-CDR, so the comparison there is close to 'REMIND against IMAGE, MESSAGEix, WITCH and GCAM'.",
   "IT IS PLAUSIBLY SUBSTANTIVE, NOT CLERICAL. At the most stringent target, frameworks diverge structurally in HOW they get there — REMIND builds renewables, IMAGE and WITCH lean on removal — and that divergence is what sorts them into arms. The confound and the finding share a cause, which is worth saying rather than treating the imbalance as a data-quality nuisance.",
   "THE WITHIN-MODEL CHECK SPLITS BY AMBITION. On jobs, 72% of family-cell comparisons point the pooled way and the median family agrees in 16 of 20 cells. At 2°C four families can be asked and jobs is solid (World 2°C: four of four agree). At 1.5°C only MESSAGEix holds both arms, on three High-RE scenarios, so those cells rest on pooling.",
   "DEPRIVATION IS THE OUTCOME MOST EXPOSED TO IT. Only 42% of family comparisons agree with the pooled sign, and Simpson's paradox runs both ways: Europe, Latin America and Africa read FOR High-RE pooled and AGAINST inside the models at 2°C, while the Middle East and Rest of Asia do the reverse. Mortality is thinner still — two families, 25% agreement."],
  {t:"THE CONFOUND",c:RED},
  "Sources: W14_within_model_landprimary.R for the within-model grid and W15_arm_composition.R for the composition and leave-one-out. Effect size in the within-model test is Cliff's delta: inside one family the samples are tiny and the question is about rank overlap, not the size of the gap.");

table("Is it real?","What survives, and what the reader should take on trust",
  ["","Direction (the claim)","Interval (context)","Leave-one-out (sensitivity)","How to state it"],
  [[{t:"Energy jobs",o:{bold:true}},{t:"20 of 20 point to High-RE",o:{bold:true,color:GREEN}},
    {t:"all 20 clear",o:{color:GREEN}},{t:"19 of 20 keep direction",o:{color:GREEN,bold:true}},
    {t:"a result",o:{bold:true,color:GREEN}}],
   [{t:"Energy deprivation",o:{bold:true}},{t:"14 of 20 point to High-RE",o:{color:GOLD}},
    {t:"11 clear · 2 against",o:{color:GOLD}},{t:"8 of 20 reverse",o:{color:RED}},
    {t:"an association; regionally uneven",o:{color:GOLD}}],
   [{t:"PM2.5 mortality",o:{bold:true}},{t:"8 of 20 point to High-RE",o:{color:GOLD}},
    {t:"5 clear · 2 against",o:{color:GOLD}},{t:"untestable at 1.5°C",o:{color:RED}},
    {t:"Europe and World 1.5°C only",o:{color:GOLD}}]],
  [2.4,2.9,1.9,2.6,2.3],{t:"VERDICT",c:GOLD},
  "THE FIRST COLUMN IS THE CLAIM; THE OTHER TWO BOUND IT. Employment points one way in every cell, clears every interval, and holds under the leave-one-out sensitivity — it can be stated as a result. Energy access and air quality point favourably on average, clear in about half their cells, and are more exposed to model composition, so they are stated as associations with the sensitivity attached. The axis choice carries none of this: on the engineered-only axis the direction counts are jobs 20/20, deprivation 15/20, health 9/20.");

bullets("Sensitivity — leave one out","What happens if the model dominating the High-RE arm is removed",
  ["WHY THIS IS A SENSITIVITY, NOT A RESULT. The two arms are not drawn from the same mix of models: High-CDR is a genuine ensemble (effectively 3.6 models at 1.5°C) while High-RE is effectively 1.3, because REMIND supplies 88% of it. That is a property of what AR6 contains and which models report which pathways — an artefact of the database, not of this analysis.",
   "READ IT AS A CEILING ON CONFIDENCE. Removing REMIND leaves 8 High-RE scenarios at 1.5°C and 73 at 2°C, so intervals widen and significance is lost almost everywhere. That part is lost power and should be discounted. A change of SIGN cannot be, because lost power widens an interval around a stable estimate rather than moving it.",
   "THE EMPLOYMENT DIRECTION SURVIVES. 19 of 20 cells keep their sign, and World 2°C still clears on 73 non-REMIND scenarios. The magnitude falls to a median 18% of its pooled value, so report the pooled figure as the central estimate and this as a conservative floor.",
   "DEPRIVATION AND MORTALITY ARE MORE EXPOSED. Deprivation reverses in 8 of 20 cells; mortality cannot be scored at 1.5°C at all and reverses in 2 of 10 scoreable 2°C cells. This bounds how firmly those two can be attributed to the pathway — it does not change the pooled directions reported in Part three."],
  {t:"LEAVE ONE OUT",c:TEAL},
  "Source: W15_arm_composition.R. Every cell is recomputed on identical outcome data with one family removed from both arms; the script self-checks that it reproduces the pooled grid before dropping anything.");

table("Sensitivity — leave one out","Drop REMIND from the High-RE arm: the jobs direction holds, the others do not",
  ["World cell","With REMIND","Without REMIND","Residual RE arm","What survives"],
  [[{t:"Jobs · 1.5°C",o:{bold:true}},{t:"+463.9 ●",o:{color:GREEN,bold:true}},{t:"+84.0",o:{color:MUTE,bold:true}},"8 scenarios",
    {t:"sign holds; 18% of the gap; interval lost",o:{color:GOLD}}],
   [{t:"Jobs · 2°C",o:{bold:true}},{t:"+284.7 ●",o:{color:GREEN,bold:true}},{t:"+37.4 ●",o:{color:GREEN,bold:true}},"73 scenarios",
    {t:"STILL SIGNIFICANT, at 13% of the gap",o:{color:GREEN}}],
   [{t:"Deprivation · 1.5°C",o:{bold:true}},{t:"−6.81 ●",o:{color:GREEN,bold:true}},{t:"+2.77",o:{color:RED,bold:true}},"8 scenarios",
    {t:"SIGN FLIPS",o:{color:RED,bold:true}}],
   [{t:"Deprivation · 2°C",o:{bold:true}},{t:"−4.09",o:{color:MUTE}},{t:"+4.31",o:{color:RED,bold:true}},"65 scenarios",
    {t:"SIGN FLIPS",o:{color:RED,bold:true}}],
   [{t:"Mortality · 1.5°C",o:{bold:true}},{t:"−11.11 ●",o:{color:GREEN,bold:true}},{t:"—",o:{color:MUTE}},"2 scenarios",
    {t:"cannot be scored at all",o:{color:MUTE}}],
   [{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}}],
   [{t:"ACROSS ALL NINE REGIONS + WORLD",o:{bold:true,color:TEAL}},"","","",""],
   [{t:"  Jobs — cells keeping the sign",o:{bold:true}},"20 of 20",{t:"19 of 20",o:{color:GREEN,bold:true}},"",
    {t:"median 18–19% of the advantage retained",o:{color:GOLD}}],
   [{t:"  Jobs — cells still significant",o:{bold:true}},"20",{t:"9",o:{color:GOLD,bold:true}},"","7 of those 9 are at 2°C"],
   [{t:"  Deprivation — cells still significant",o:{bold:true}},"13",{t:"0",o:{color:RED,bold:true}},"",
    {t:"8 of 20 cells flip sign",o:{color:RED,bold:true}}]],
  [3.0,1.9,1.9,1.7,3.6],{t:"LEAVE-ONE-OUT",c:RED},
  "READ THIS CAREFULLY IN BOTH DIRECTIONS. Dropping REMIND leaves 8 High-RE scenarios at 1.5°C and 73 at 2°C, so losing significance is partly just lost power and is NOT by itself evidence of a confound. But a power loss widens the interval around a STABLE estimate — it does not move the estimate by 80%, and it does not flip signs. Both happen here. The defensible reading: the DIRECTION of the jobs result survives the loss of its dominant model, and at 2°C it survives significantly on 73 independent scenarios, so there is a real pathway effect — but the SIZE of the pooled advantage is substantially a REMIND effect, and the deprivation result does not survive at all.",9.5);




table("Sensitivity — leave one out","Mortality cannot be checked against its dominant model at all",
  ["Cell","With REMIND","Without REMIND","Residual RE arm","Verdict"],
  [[{t:"World · 1.5°C",o:{bold:true}},{t:"−11.11 ●",o:{color:GREEN,bold:true}},{t:"cannot score",o:{color:MUTE,bold:true}},{t:"2 scenarios",o:{color:RED}},
    {t:"UNTESTABLE",o:{color:RED,bold:true}}],
   [{t:"Europe · 1.5°C",o:{bold:true}},{t:"+4.99 ●",o:{color:GREEN,bold:true}},{t:"cannot score",o:{color:MUTE,bold:true}},{t:"2 scenarios",o:{color:RED}},
    {t:"UNTESTABLE — and this is the headline claim",o:{color:RED,bold:true}}],
   [{t:"Europe · 2°C",o:{bold:true}},{t:"+3.66 ●",o:{color:GREEN,bold:true}},{t:"+0.23",o:{color:RED,bold:true}},"27 scenarios",
    {t:"94% of the effect gone; not significant",o:{color:RED}}],
   [{t:"Africa · 2°C",o:{bold:true}},{t:"−2.41 ●",o:{color:RED,bold:true}},{t:"+0.45",o:{color:MUTE}},"27 scenarios",{t:"SIGN FLIPS",o:{color:RED,bold:true}}],
   [{t:"Reforming econ. · 2°C",o:{bold:true}},{t:"+0.89 ●",o:{color:GREEN,bold:true}},{t:"−0.52",o:{color:MUTE}},"27 scenarios",{t:"SIGN FLIPS",o:{color:RED,bold:true}}],
   [{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}}],
   [{t:"All ten 1.5°C cells",o:{bold:true}},"4 significant",{t:"cannot be scored at all",o:{color:RED,bold:true}},{t:"2 scenarios",o:{color:RED}},
    {t:"the test cannot be run at high ambition",o:{color:RED,bold:true}}],
   [{t:"All ten 2°C cells",o:{bold:true}},"3 significant",{t:"0 of 3 survive · 2 flip",o:{color:RED,bold:true}},"27 scenarios",
    {t:"one NEW significant cell appears — on 27 scenarios, noise",o:{color:MUTE}}]],
  [2.9,2.0,2.4,1.8,3.0],{t:"MORTALITY",c:RED},
  "BE FAIR ABOUT WHAT THIS DOES AND DOES NOT SHOW. Twenty-seven High-RE scenarios from four families is thin, so 'does not survive' here means CANNOT BE VERIFIED, not 'is refuted'. But the pattern matches jobs and deprivation: point estimates move by 90%+ and signs flip, which is not what lost power alone looks like. The practical consequence is concrete — the paper currently leads its air-quality section with Europe, and the Europe result is the one claim in the study that cannot be checked against its dominant model at all at 1.5°C.",9.5);



bullets("Sensitivity — the axis boundary","What happens if the CDR axis is narrowed to engineered removal only",
  ["THE QUESTION. The primary axis is all CDR. Narrowing it to engineered removal — novel CDR plus fossil and industrial CCS, excluding afforestation, reforestation and soil carbon — is the obvious alternative, and it is what an earlier version of this analysis used. The test re-runs the entire classification on the narrower axis and scores both label sets on IDENTICAL outcome data, so only the labels differ.",
   "OF THE SCENARIOS BOTH AXES CLASSIFY, NOT ONE SWITCHES ARMS. 530 scenarios are classified under both definitions and 100% keep the same label. The two axes differ by WHICH scenarios they admit — the all-CDR axis adds 80 and drops 76 — not by how they label a shared one. The axes correlate 0.898.",
   "THE NARROWER AXIS IS THE CONSERVATIVE ONE, WHICH IS THE POINT. It reports a SMALLER High-RE jobs advantage at World 1.5°C (+405 against +464) and smaller avoided mortality (8.7 million against 11.1), because land-heavy CDR scenarios carry fewer energy jobs. So the headline is not resting on the wider definition: the definition that flatters High-RE least still gives the same answer, significantly, in every jobs cell.",
   "WHY ALL-CDR IS NEVERTHELESS THE RIGHT PRIMARY. The paper asks whether renewables-led mitigation beats CDR-led mitigation. Land-based removal is CDR. An axis that excludes it names a subset of the comparison arm and, in Latin America, makes carbon management nearly invisible in a region whose removal is overwhelmingly land-based. The honest reading is that the choice of axis is a definitional question, both answers are reported, and they agree."],
  {t:"AXIS IN / OUT",c:TEAL},
  "Source: V5_land_primary.R, which builds both label sets from the deployment file with the published rule and reproduces the published classification exactly for the full database. The sample is held fixed across the two runs so that only the axis differs, never the set of scenarios entering the tercile.");

img("Sensitivity — the axis boundary","Every cell, both axes, on the same outcome data",FIG+"F3_land.png",
  "Dark point is the primary all-CDR result; the arrow head is the engineered-only sensitivity.",
  "Positive favours High-RE. The arrows are short and mostly point the same way. No jobs cell changes sign, and where the engineered axis differs it generally reports a SMALLER advantage — which is why it is the conservative specification rather than a threat to the result.",
  {t:"FIGURE F3",c:TEAL},0.595);

bullets("Sensitivity — the zeros","Some of the High-CDR arm never reported renewables at all",
  ["THE PROBLEM. The classification pivots renewable capacity with a zero fill, so a scenario that never reports Renewable Capacity is scored as deploying none. That puts it at the bottom of the RE distribution and makes it eligible for High-CDR. On the all-CDR axis, 14 of the 67 High-CDR scenarios at 1.5°C and 28 of 238 at 2°C sit at exactly zero renewables. There are NONE in the High-RE arm, by construction: a scenario cannot be top-tercile on renewables while reporting none.",
   "THEY ARE MISSING DATA, NOT ZEROS. Every one was checked against the cumulative deployment file for a Renewable Capacity ROW. None has a row at all. Not one is a reported zero. They are concentrated in GCAM, which is a reporting convention rather than a modelling result — and the SCI-vetted sample already excludes every one of them.",
   "AND THEY ARE NOT NEUTRAL. Their median World net energy employment sits near zero, against roughly 300 job-years per 1,000 for the High-CDR scenarios that do report. They drag the High-CDR arm down on the outcome, on the strength of an axis value that the fill invented.",
   "WHAT DROPPING THEM WOULD DO. Jobs weakens slightly and still clears easily. Deprivation strengthens. On the engineered axis this was the difference between a non-significant and a significant World deprivation result; on the all-CDR axis the World deprivation cell at 1.5°C ALREADY clears, so the decision no longer carries a headline. It remains a labelled sensitivity and a reporting choice, not a technical one."],
  {t:"THE ZEROS",c:RED},
  "Source: W13_zeros_and_land_mortality.R for the diagnosis, and the primary grid for the arm counts. Whichever way this is resolved, the direction of every World cell is unchanged — dropping the zeros makes the deprivation result stronger, never weaker.");

img("Sensitivity — the zeros","What the non-reporting scenarios do to the High-CDR arm",FIG+"F5_zeros.png",
  "World net energy employment for the High-CDR arm only, split by whether the scenario reported renewable capacity.",
  "The non-reporting group sits at roughly zero net energy employment while the reporting group sits around 300 job-years per 1,000. The zero fill is not a harmless default: it recruits scenarios into the comparison arm on an axis value they never supplied.",
  {t:"FIGURE F5",c:RED},0.5);

section("Part seven","What we can claim","And what has to be said alongside it.");

bullets("The claim","What renewables-led mitigation delivers",
  ["AT WORLD, SEVEN OF EIGHT CELLS POINT TO HIGH-RE — energy employment, the decent-living gap and the deprived headcount at both ambition levels, and PM2.5 mortality at 1.5°C. The single exception is mortality at 2°C. Across regions, 42 of 60 comparisons point to High-RE.",
   "ONE — WORK, AND IT IS UNANIMOUS. High-RE leads in ALL 20 region × ambition cells and all 20 clear the interval; no other outcome does either. World net energy employment runs 228 against 692 job-years per 1,000 at 1.5°C (+204%) and 187 against 471 at 2°C (+153%). The mechanism is the renewable build, which outweighs High-CDR's fossil, nuclear and bioenergy employment combined by 11 to 1.",
   "TWO — ENERGY ACCESS. The World decent-living gap runs 18.43 against 11.62 GJ per capita at 1.5°C and 15.46 against 11.37 at 2°C, with the deprived headcount moving the same way. It leads in 14 of 20 cells — Africa, Latin America, Europe, North America, India+ and China+ — and reverses in Rest of Asia and the Middle East.",
   "THREE — AIR QUALITY, THE UNEVEN ONE. High-RE avoids 11.11 million cumulative deaths at World 1.5°C, and Europe carries it: 4.99 and 3.66 million avoided, a 31% reduction. But only 8 of 20 cells point to High-RE, Africa points the other way at both levels — household solid fuel and dust, not the power mix — and World 2°C is 4.3 million higher. No universal co-benefit to claim."],
  {t:"CLAIM",c:GOLD});

bullets("The claim","How firmly each can be stated",
  ["EMPLOYMENT IS THE FINDING. Unanimous in direction across every region and ambition level, significant in every cell, mechanistically explained, and it holds under the leave-one-out sensitivity in 19 of 20 cells. State it as a result. Report the pooled magnitude as the central estimate and the non-REMIND figure as a conservative floor, because the size — unlike the direction — is largely carried by one model.",
   "ENERGY ACCESS AND AIR QUALITY ARE ASSOCIATIONS. Both point favourably on average and both are regionally uneven, with named reversals that have plausible mechanisms rather than looking like noise. Both are also more exposed to which models populate which arm. State them as associations this database supports, with the regional pattern given explicitly and the sensitivities in Part six attached.",
   "THE REVERSALS ARE PART OF THE RESULT, NOT AN EMBARRASSMENT. The Middle East widening its energy gap, Rest of Asia at 2°C, and Africa's air quality all have coherent explanations — an export economy, a heterogeneous region, and a PM2.5 burden the power sector does not control. A paper that reported three uniform benefits would be less credible than one that reports where the pattern breaks and why.",
   "AND THE SPECIFICATION IS NOT WHAT CARRIES ANY OF IT. On the engineered-only CDR axis the direction counts are jobs 20/20, deprivation 15/20, mortality 9/20 — materially the same picture from a narrower and more conservative definition."],
  {t:"CLAIM",c:GOLD},
  "Direction is the claim throughout; intervals are reported as context on how firmly each cell points.");

bullets("Limitations","Stated plainly, because they are load-bearing",
  ["MODEL COMPOSITION IS THE BINDING CONSTRAINT, AND IT IS AN ARTEFACT OF WHAT AR6 CONTAINS. The two arms are not drawn from the same mix of models: the High-CDR arm is a genuine ensemble (effectively 3.6 models at 1.5°C, 4.7 at 2°C) while the High-RE arm is effectively 1.3 and 2.0, because REMIND supplies 88% and 69% of it. This is a property of which models report which pathways, not of the analysis — but it bounds attribution. The leave-one-out sensitivity in Part six quantifies it: the employment direction survives in 19 of 20 cells; deprivation and mortality are more exposed.",
   "TWO UPSTREAM DATA DEFECTS WERE FOUND AND FIXED, AND BOTH MATTERED. The World row was aggregated within deployment-variable groups, so it inherited the wrong regional coverage; and 71 classified scenarios never reached the outcome tables because the labels file stores degree signs as literal text that no join could match. Both are fixed here, both moved numbers, and neither reversed a finding — but they are a reminder that the published intermediate files should not be taken at face value.",
   "MORTALITY RUNS ON A RESTRICTED SAMPLE, AND ON THIS AXIS THE RESTRICTION IS WORSE. Its target list was drawn against the ENGINEERED labels, so the 80 scenarios the all-CDR axis newly admits have no mortality run at all. That is target selection rather than a property of those scenarios, and it means the mortality arms here are smaller than the engineered axis's. Re-running the mortality targets against the all-CDR labels is the single most valuable outstanding job.",
   "THE DEPRIVATION MEASURE TRUNCATES AT ZERO, so it responds only to sectors where a region falls short — not always the household sector. It is a regional aggregate and cannot speak to who inside a region is deprived.",
   "THE OUTCOME SET IS ENERGY-SYSTEM CENTRIC. Now that land-based removal is inside the CDR axis, this cuts harder: the analysis scores land-heavy pathways on jobs, energy access and air quality while saying nothing about land competition, food prices, tenure or biodiversity, which are the channels through which land-based removal most plausibly affects wellbeing. The comparison is fair on what it measures and silent on what it does not."],
  {t:"LIMITS",c:RED});

bullets("What is open","Next steps, in priority order",
  ["RE-RUN THE MORTALITY TARGETS AGAINST THE ALL-CDR LABELS, AND CHECK WHAT IT DOES TO COMPOSITION. 80 newly classified scenarios carry no mortality output because the targets were drawn under the engineered axis. Worth doing on its own terms — but the more important question is whether a re-run recovers any non-REMIND breadth in the High-RE arm, because at present that arm is 95% one model at 1.5°C and no air-quality direction can be tested there.",
   "REPORT THE JOBS MAGNITUDE WITH ITS LEAVE-ONE-OUT VALUE ALONGSIDE. The direction is unanimous and survives; the pooled size is largely REMIND. A reviewer will ask, and giving both figures in the same sentence pre-empts it — the pooled estimate as the central value, the non-REMIND estimate as a conservative floor.",
   "FIX THE LABELS FILE AT SOURCE. The mangled degree signs should be corrected where the file is written, not repaired downstream, and the published summary's inner_join() should be made to fail loudly on unmatched keys rather than dropping them silently.",
   "DECIDE HOW TO REPORT THE ZERO-RENEWABLE SCENARIOS. Retained with the zero fill is the published behaviour. On the all-CDR axis dropping them no longer changes a headline, so this is now a presentational choice rather than a live risk.",
   "RE_SPEC DEFINITION SENSITIVITY — whether the renewables axis should include nuclear or biomass. A reviewer will ask, and the answer is currently a principled argument without a table behind it."],
  {t:"OPEN",c:TEAL});

// Written into decks/ rather than the working directory, for the same reason the
// figures are: running this from the repo root should not leave files there.
const OUT = (process.env.COMPASS_DECK || "decks/COMPASS_Paper1_final_8.25.pptx");
require("fs").mkdirSync(require("path").dirname(OUT), {recursive:true});
p.writeFile({fileName:OUT}).then(()=>console.log("slides:",N,"->",OUT));
