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
  s.addText("Engineered carbon management or high renewables?",
    {x:0.9,y:2.45,w:11.4,h:1.5,fontSize:40,bold:true,color:"FFFFFF",fontFace:FH,lineSpacing:46});
  s.addText("Wellbeing outcomes across nine world regions and two levels of ambition",
    {x:0.9,y:4.05,w:10.6,h:0.5,fontSize:16,color:"C7D6E0",fontFace:F});
  s.addShape(p.ShapeType.rect,{x:0.9,y:4.75,w:2.2,h:0.03,fill:{color:GOLD}});
  s.addText("606 classified scenarios · 10 model families · 3 outcome families · 60 comparisons · cumulative 2020–2100",
    {x:0.9,y:5.0,w:11,h:0.34,fontSize:12,color:"9FB6C4",fontFace:F});
  s.addText("CMT axis = Novel CDR + fossil/industrial CCS, land-based removal excluded",
    {x:0.9,y:5.35,w:11,h:0.34,fontSize:11,color:"7FB3C8",fontFace:F,italic:true});
}

bullets("The answer in one slide","Renewables-led mitigation delivers more wellbeing — decisively in one outcome",
  ["ENERGY EMPLOYMENT — the result, and it is now unanimous. High-RE is better in all 20 region × ambition cells and ALL 20 clear the interval. At World, net energy employment goes from 291 to 687 job-years per 1,000 people at 1.5°C (+136%) and 173 to 466 at 2°C (+169%). Nothing else in the study comes close for consistency.",
   "ENERGY DEPRIVATION — directionally positive, but the World cell no longer clears zero. The decent-living gap closes from 18.67 to 12.98 GJ per capita at 1.5°C, a 30% reduction — but the interval is [−1.21, +12.48]. Better in 13 of 20 cells, 6 significant, 3 significantly against. It holds firmly in Latin America, Europe, India+ and China+, and reverses in the Middle East.",
   "AIR-QUALITY MORTALITY — now measurable, and mildly favourable. Restricting to scenarios that report all five PM2.5 precursors directly at R10 level removes the ammonia problem at source. High-RE avoids 8.69 million cumulative deaths at World 1.5°C [+0.97, +46.90] — significant — while the 2°C cell is −3.03 and not. Europe carries it: 4.61 million avoided, significant at both levels.",
   "OVERALL: 42 of 60 comparisons favour High-RE, 30 significantly, 4 against. The revamp made jobs stronger, deprivation weaker at World, and mortality reportable for the first time."],
  {t:"SUMMARY",c:GOLD},
  "Cumulative 2020–2100 for both classification and outcomes. Every cell is the two arm medians with a 2,000-replicate cluster bootstrap on the raw difference, over 290 model × scenario-family clusters.");

table("The answer in one slide","World, all three outcomes, both ambition levels",
  ["Outcome","Ambition","High-CMT","High-RE","Difference","95% interval","Change"],
  [[{t:"Energy jobs",o:{bold:true}},"1.5°C","290.97","686.78",{t:"+395.81",o:{color:GREEN,bold:true}},{t:"[+285, +508]",o:{color:GREEN}},{t:"+136%",o:{color:GREEN,bold:true}}],
   [{t:"job-years per 1,000",o:{color:MUTE}},"2°C","173.29","465.97",{t:"+292.68",o:{color:GREEN,bold:true}},{t:"[+147, +411]",o:{color:GREEN}},{t:"+169%",o:{color:GREEN,bold:true}}],
   [{t:"Energy deprivation",o:{bold:true}},"1.5°C","18.67","12.98",{t:"−5.69",o:{bold:true}},{t:"[−1.21, +12.48]",o:{color:MUTE,bold:true}},"−30%"],
   [{t:"gap, GJ per capita",o:{color:MUTE}},"2°C","15.58","11.27",{t:"−4.31",o:{bold:true}},{t:"[−0.38, +8.05]",o:{color:MUTE,bold:true}},"−28%"],
   [{t:"PM2.5 mortality",o:{bold:true}},"1.5°C","423.82","415.12",{t:"−8.69",o:{color:GREEN,bold:true}},{t:"[+0.97, +46.90]",o:{color:GREEN}},{t:"−2%",o:{color:GREEN}}],
   [{t:"million cumulative deaths",o:{color:MUTE}},"2°C","433.70","436.73",{t:"+3.03",o:{color:MUTE}},{t:"[−19.4, +29.8]",o:{color:MUTE}},"+1%"]],
  [2.6,1.2,1.4,1.4,1.7,2.2,1.6],{t:"WORLD",c:GOLD},
  "A positive interval favours High-RE throughout. Jobs clears by a wide margin at both levels; deprivation is large but its interval touches zero; mortality clears at 1.5°C only. Mortality runs on the reporting-complete sample (42 vs 37 at 1.5°C), which is why its arms are smaller.");

bullets("What changed in the revamp","Four upstream changes, and what each one fixes",
  ["THE CMT AXIS IS NOW ENGINEERED REMOVAL — Novel CDR plus fossil and industrial CCS, with LAND-BASED CDR EXCLUDED. The most consequential change. The old Total CDR axis put afforestation and soil carbon in the same bucket as DACCS and BECCS, so the Latin America arm (73% land-based previously) was scored on an intervention the outcome set cannot see. Excluding land makes this a contrast between two ENERGY-SYSTEM strategies, which is what the outcomes measure.",
   "THE WINDOW IS 2020–2100 for both classification and outcomes, up from 2020–2050. Jobs numbers are larger in absolute terms and include more of the operational phase rather than only the construction surge.",
   "THE ARMS ARE BALANCED BY CONSTRUCTION: 64/64 at 1.5°C and 239/239 at 2°C, against 74/62 and 261/193 before. Balanced arms remove one asymmetry from every comparison.",
   "MORTALITY USES ONLY DIRECTLY-REPORTED R10 PRECURSORS — all five of SO₂, NOₓ, BC, OM and NH₃, with no World-to-R10 disaggregation and no ammonia sidecar. This resolves BOTH open problems from the previous round at once: the ammonia reporting asymmetry and the synthetic-regions risk are handled by dropping the scenarios that caused them rather than patching. 422 of 643 classified targets produce complete output; 150 are rejected rather than filled."],
  {t:"THE REVAMP",c:TEAL},
  "The previous deck's entire section on ammonia harmonisation is now obsolete — the input rule makes the correction unnecessary. That is a better fix than the one we had.");

section("Part one","Methodology","How each wellbeing outcome is built.");

table("Method","The whole design on one slide",
  ["Step","Choice","Why this and not something else"],
  [[{t:"1. Ambition",o:{bold:true}},"AR6 category → 1.5°C (C1+C2) and 2°C (C3+C4)","Compares pathways reaching the SAME climate outcome. Without it the comparison is just ambition."],
   [{t:"2. CMT axis",o:{bold:true}},"Novel CDR + fossil/industrial CCS · land excluded","Engineered removal only. Land-based removal is a different intervention with land-competition consequences this outcome set cannot measure."],
   [{t:"3. RE axis",o:{bold:true}},"cumulative renewable capacity","Wind, solar, hydro, geothermal. Nuclear and biomass excluded — biomass is the substrate of BECCS, so counting it would let one scenario score on both axes."],
   [{t:"4. Labels",o:{bold:true}},"top tercile on the focal axis, NOT top tercile on the opposing axis","Scenarios high on both are genuinely both and are dropped; high on neither are neither. Yields balanced arms."],
   [{t:"5. Window",o:{bold:true}},"cumulative 2020–2100","Full scenario horizon, for both the classification and the outcomes."],
   [{t:"6. Inference",o:{bold:true}},"cluster bootstrap on the raw difference in medians","606 classified scenarios sit in 290 model × scenario-family clusters. 2,000 replicates resampling whole clusters."]],
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
   [{t:"Grouping",o:{bold:true}},"renewables = wind · solar · hydro · geothermal","Nuclear and bioenergy are tracked separately and BOTH favour High-CMT (−17 and −44 job-years per 1,000 at World 1.5°C). Biomass is excluded from the renewable axis because it is the substrate of BECCS — counting it would let one scenario score on both classification axes"]],
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

table("The sample","From the database to 606 classified scenarios",
  ["","Count","Note"],
  [["Scenarios carrying the classification variables","1,379","must report engineered CDR components and renewable capacity"],
   [{t:"Classified, full database (A)",o:{bold:true}},{t:"606",o:{bold:true}},"64 + 64 at 1.5°C · 239 + 239 at 2°C — balanced by construction"],
   ["Classified, SCI-vetted (C)","140","7 + 7 at 1.5°C · 63 + 63 at 2°C"],
   ["Model families · holding both arms",{t:"10 · 8",o:{bold:true,color:GREEN}},"up from 9 families with 7 holding both arms"],
   ["Model × scenario-family clusters","290","the unit resampled by the bootstrap"],
   ["Reaching complete mortality output","422 of 643","150 rejected rather than filled"]],
  [4.8,2.0,5.3],{t:"FUNNEL",c:TEAL},
  "The portfolio rule — top tercile on the focal axis and NOT top tercile on the opposing axis — produces equal arms automatically. That removes an asymmetry the previous design had to carry through every robustness test.");

table("Model composition","Who populates which arm — and it has genuinely improved",
  ["Model family","High-CMT","High-RE","Holds both arms?","Note"],
  [[{t:"REMIND",o:{bold:true}},{t:"3",o:{color:CMT}},{t:"216",o:{bold:true,color:RE}},{t:"barely",o:{color:RED}},"still 71% of the High-RE arm"],
   [{t:"MESSAGEix",o:{bold:true}},{t:"44",o:{bold:true}},{t:"37",o:{bold:true}},{t:"YES — balanced",o:{color:GREEN,bold:true}},"the key change: 37 High-RE against 4 previously"],
   ["IMAGE","66","2",{t:"barely",o:{color:RED}},""],
   ["POLES-JRC","57","9",{t:"yes",o:{color:GREEN}},""],
   ["GCAM","39","5",{t:"yes",o:{color:GREEN}},""],
   ["AIM/CGE","28","10",{t:"yes",o:{color:GREEN}},""],
   ["GEM","25","0",{t:"no",o:{color:RED}},"new family in this run"],
   ["WITCH","8","16",{t:"yes",o:{color:GREEN}},""],
   ["TIAM-ECN","21","0",{t:"no",o:{color:RED}},""],
   ["COFFEE","12","8",{t:"yes",o:{color:GREEN}},""]],
  [2.4,1.5,1.5,2.4,4.3],{t:"IMPROVED",c:GOLD},
  "REMIND is 71% of High-RE and 1% of High-CMT — down from 85% / 1%. Eight of ten families hold both arms, and MESSAGEix holds 44 against 37, which is genuinely balanced. The confound is materially reduced but not eliminated, so the within-model check still decides what the paper can claim.",10.5);

table("Diagnostics","World coverage flow — how many scenarios each outcome can actually use",
  ["","1.5°C CMT","1.5°C RE","2°C CMT","2°C RE","What gates it"],
  [[{t:"Classified",o:{bold:true}},{t:"64",o:{bold:true}},{t:"64",o:{bold:true}},{t:"239",o:{bold:true}},{t:"239",o:{bold:true}},
    "Top tercile on the focal axis, not top tercile on the opposing axis"],
   [{t:"Complete World jobs",o:{bold:true}},"42","43","192","218",
    "Requires renewable AND fossil jobs in all ten R10 regions"],
   [{t:"Complete World deprivation",o:{bold:true}},"53","43","189","210",
    "Requires the decent-living gap in all ten regions"],
   [{t:"Complete World mortality",o:{bold:true}},{t:"42",o:{color:RED}},{t:"37",o:{color:RED}},{t:"138",o:{color:RED}},{t:"168",o:{color:RED}},
    "Requires all five PM2.5 precursors reported directly at R10 — the strictest gate"],
   [{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}},{t:"",o:{}}],
   [{t:"Retention, jobs",o:{color:MUTE}},{t:"66%",o:{color:MUTE}},{t:"67%",o:{color:MUTE}},{t:"80%",o:{color:MUTE}},{t:"91%",o:{color:MUTE}},
    {t:"The three outcomes are gated INDEPENDENTLY — missing mortality does not blank jobs",o:{color:MUTE}}],
   [{t:"Retention, mortality",o:{color:MUTE}},{t:"66%",o:{color:MUTE}},{t:"58%",o:{color:MUTE}},{t:"58%",o:{color:MUTE}},{t:"70%",o:{color:MUTE}},
    {t:"Mortality is always the binding constraint on sample size",o:{color:MUTE}}]],
  [3.2,1.5,1.5,1.5,1.5,3.0],{t:"COVERAGE",c:TEAL},
  "A World figure is computed ONLY when all ten R10 regions are present for that outcome. Partial-region sums are set to NA rather than reported, which is why the arm sizes differ between outcomes and why they are smaller than the classified counts. This is the fix described on the next slide; the previous World row inherited whichever deployment variable's coverage its table row belonged to.",10);

img("Diagnostics","The same coverage flow, drawn",FIG+"F4_coverage.png",
  "Each outcome is gated on its own ten-region completeness, so the arms differ in size between outcomes.",
  "Deprivation retains MORE High-CMT scenarios than jobs at 1.5°C (53 against 42), which is exactly why the three outcomes have to be gated separately rather than jointly. Mortality is the binding constraint everywhere.",
  {t:"FIGURE F4",c:TEAL},0.5);

bullets("Diagnostics","The World aggregation fix, and what it moved",
  ["THE DEFECT. The master built the World row by summing outcomes WITHIN each deployment-variable group, so a scenario's World jobs total inherited the regional coverage of whichever CDR variable that row belonged to. COFFEE 1.1 / COMMIT-Baseline reports Renewable Capacity for ten regions and Total CDR for nine, and its World jobs read 765,457 on one row and 684,824 on the other. All 288 discrepant scenario-regions showed exactly this pattern — 288 of 288.",
   "THE FIX. World outcomes are now built from an outcome-only R10 table with no reference to deployment rows, each outcome gated INDEPENDENTLY on having all ten regions, with explicit coverage fields carried through. The R10 rows were verified clean — zero variation across Variable rows — so the corruption was created entirely in the aggregation step and World could be rebuilt without re-running the master.",
   "WHAT IT MOVED, and it is less than the raw sensitivity suggested. World jobs at 1.5°C is UNCHANGED at 686.78. At 2°C it moves from 461.44 to 465.97 and the gap from +288.3 to +292.7. Deprivation at 2°C moves from 11.06 to 11.27. Every regional cell is unchanged by construction, and the scorecard is unchanged at 42 of 60.",
   "MORTALITY NEEDED NO FIX. The reporting-complete pipeline already implements this rule: all 516 scenarios carrying a World row have exactly ten regions, and World equals the sum of the ten to within 1e-13. Requiring direct R10 precursor reporting enforces completeness upstream."],
  {t:"THE FIX",c:GREEN},
  "Implemented and validated downstream in V3_world_strict.R, then PORTED INTO THE MASTER — build_df_master() in COMPASS_master_analysis_allR10.R now carries the strict rule and the coverage fields. V4_verify_port.R extracts the ported block from the master verbatim, runs it on the published R10 rows and reproduces V3 to machine precision (max |difference| 0). Re-running from source now reproduces the fix, not the defect.");

section("Part three","The wellbeing results","What High-RE delivers, at World and in every region.");

img("Result — the whole grid","All 60 comparisons on one panel",FIG+"F1_scorecard.png",
  "Every region × ambition × outcome cell, coloured by direction, with the saturated fill marking the cells whose interval clears zero.",
  "The jobs column is solid: twenty of twenty, all significant. Deprivation and mortality are patchy by region, which is the paper's actual finding rather than a weakness in it. Pacific OECD is excluded from the regional rows and retained inside the World aggregate.",
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
  ["Region","High-CMT","High-RE","Difference","High-CMT","High-RE","Difference"],
  [["WORLD","290.97","686.78",{t:"+395.8 ●",o:{color:GREEN,bold:true}},"173.29","465.97",{t:"+292.7 ●",o:{color:GREEN,bold:true}}],
   ["Africa","225.80","1057.55",{t:"+831.8 ●",o:{color:GREEN,bold:true}},"167.37","705.78",{t:"+538.4 ●",o:{color:GREEN,bold:true}}],
   ["China+","209.47","333.70",{t:"+124.2 ●",o:{color:GREEN}},"157.68","248.68",{t:"+91.0 ●",o:{color:GREEN}}],
   ["Europe","91.50","154.49",{t:"+63.0 ●",o:{color:GREEN}},"57.00","114.89",{t:"+57.9 ●",o:{color:GREEN}}],
   ["India+","268.02","1000.64",{t:"+732.6 ●",o:{color:GREEN,bold:true}},"209.72","664.66",{t:"+454.9 ●",o:{color:GREEN,bold:true}}],
   ["Latin America","271.64","492.41",{t:"+220.8 ●",o:{color:GREEN}},"166.31","360.03",{t:"+193.7 ●",o:{color:GREEN}}],
   ["Middle East","294.18","851.03",{t:"+556.9 ●",o:{color:GREEN,bold:true}},"127.52","538.42",{t:"+410.9 ●",o:{color:GREEN,bold:true}}],
   ["North America","190.78","344.59",{t:"+153.8 ●",o:{color:GREEN}},"106.45","209.39",{t:"+102.9 ●",o:{color:GREEN}}],
   ["Reforming econ.","503.39","879.04",{t:"+375.7 ●",o:{color:GREEN}},"297.83","600.91",{t:"+303.1 ●",o:{color:GREEN}}],
   ["Rest of Asia","170.70","836.82",{t:"+666.1 ●",o:{color:GREEN,bold:true}},"98.27","540.34",{t:"+442.1 ●",o:{color:GREEN,bold:true}}]],
  "● clears a cluster-robust 95% interval on the raw difference. EVERY cell favours High-RE and EVERY cell clears the interval — the only outcome in the study of which that is true. Pacific OECD is excluded from these rows (−10.4 at 1.5°C, +18.1 at 2°C, neither significant) and retained in the World aggregate.");

regtable("Energy deprivation: decent-living gap, GJ per capita",{t:"13 / 20",c:GOLD},"",
  ["Region","High-CMT","High-RE","Gap closed","High-CMT","High-RE","Gap closed"],
  [["WORLD","18.67","12.98",{t:"−5.69",o:{bold:true}},"15.58","11.27",{t:"−4.31",o:{bold:true}}],
   ["Africa","67.27","45.15",{t:"−22.12",o:{color:MUTE}},"65.56","40.77",{t:"−24.79",o:{color:MUTE}}],
   ["China+","1.14","0.80",{t:"−0.34 ●",o:{color:GREEN}},"0.81","0.68",{t:"−0.13",o:{color:MUTE}}],
   ["Europe","5.95","2.80",{t:"−3.14 ●",o:{color:GREEN,bold:true}},"3.32","2.66",{t:"−0.66",o:{color:MUTE}}],
   ["India+","1.09","0.53",{t:"−0.56 ●",o:{color:GREEN}},"1.13","0.45",{t:"−0.68 ●",o:{color:GREEN}}],
   ["Latin America","62.94","39.81",{t:"−23.14 ●",o:{color:GREEN,bold:true}},"50.98","39.59",{t:"−11.39 ●",o:{color:GREEN,bold:true}}],
   ["Middle East","1.51","2.73",{t:"+1.23 ●",o:{color:RED,bold:true}},"0.80","2.25",{t:"+1.44 ●",o:{color:RED,bold:true}}],
   ["North America","7.62","8.51",{t:"+0.89",o:{color:MUTE}},"7.07","8.05",{t:"+0.98",o:{color:MUTE}}],
   ["Reforming econ.","1.87","1.96",{t:"+0.09",o:{color:MUTE}},"1.15","1.39",{t:"+0.24",o:{color:MUTE}}],
   ["Rest of Asia","10.65","8.33",{t:"−2.32",o:{color:MUTE}},"3.83","6.84",{t:"+3.02 ●",o:{color:RED,bold:true}}]],
  "Negative closes the gap and favours High-RE. Africa and Latin America carry the largest absolute movements, but Africa's intervals are wide ([−9.4, +46.3] and [−2.5, +42.7]) so only Latin America clears. The Middle East reverses significantly at both levels; Rest of Asia reverses at 2°C.");

regtable("PM2.5 mortality: million cumulative deaths, 2020–2100",{t:"9 / 20",c:GOLD},"",
  ["Region","High-CMT","High-RE","Deaths avoided","High-CMT","High-RE","Deaths avoided"],
  [["WORLD","423.82","415.12",{t:"+8.69 ●",o:{color:GREEN,bold:true}},"433.70","436.73",{t:"−3.03",o:{color:MUTE}}],
   ["Africa","75.37","78.51",{t:"−3.14",o:{color:MUTE}},"76.34","78.66",{t:"−2.32 ●",o:{color:RED}}],
   ["China+","78.58","77.37",{t:"+1.21",o:{color:MUTE}},"80.29","78.66",{t:"+1.63",o:{color:MUTE}}],
   ["Europe","15.76","11.16",{t:"+4.61 ●",o:{color:GREEN,bold:true}},"14.99","11.24",{t:"+3.76 ●",o:{color:GREEN,bold:true}}],
   ["India+","109.13","104.60",{t:"+4.52",o:{color:MUTE}},"114.32","116.29",{t:"−1.97",o:{color:MUTE}}],
   ["Latin America","15.25","15.66",{t:"−0.41",o:{color:MUTE}},"13.96","15.46",{t:"−1.49",o:{color:MUTE}}],
   ["Middle East","45.81","46.24",{t:"−0.43",o:{color:MUTE}},"43.80","46.65",{t:"−2.84",o:{color:MUTE}}],
   ["North America","7.71","8.92",{t:"−1.22",o:{color:MUTE}},"6.82","8.93",{t:"−2.11",o:{color:MUTE}}],
   ["Reforming econ.","14.07","13.08",{t:"+0.98 ●",o:{color:GREEN}},"14.15","13.28",{t:"+0.87",o:{color:MUTE}}],
   ["Rest of Asia","55.45","56.11",{t:"−0.67",o:{color:MUTE}},"58.40","57.89",{t:"+0.51",o:{color:MUTE}}]],
  "Positive means High-RE avoids deaths. EUROPE CARRIES THE WORLD RESULT: 4.61 and 3.76 million avoided, significant at both levels, on a base of about 15 million — a 29% reduction, the largest proportional effect anywhere. Fourteen of twenty cells straddle zero. Pacific OECD (+1.81, +1.93, both significant) is excluded from these rows.");

img("Result — World","The three World cells, with their intervals",FIG+"F2_world.png",
  "Jobs clears by a wide margin at both ambition levels; deprivation is large but touches zero; mortality clears at 1.5°C only.",
  "Each bar is the raw difference in medians with a 2,000-replicate cluster bootstrap over 290 model × scenario-family clusters. The deprivation interval is the reason that outcome is reported regionally rather than globally.",
  {t:"FIGURE F2",c:GOLD},0.417);

table("Result — the scorecard","Every design, side by side",
  ["Design","Cells","Favour High-RE","Significant for","Significant against"],
  [[{t:"Full database · 1.5°C",o:{bold:true}},"30",{t:"22",o:{bold:true}},{t:"17",o:{color:GREEN,bold:true}},"1"],
   [{t:"Full database · 2°C",o:{bold:true}},"30",{t:"20",o:{bold:true}},{t:"13",o:{color:GREEN}},"3"],
   ["SCI-vetted · 1.5°C","30",{t:"25",o:{bold:true}},"12","0"],
   ["SCI-vetted · 2°C","30","16","12","3"],
   [{t:"BY FAMILY — full database, both levels",o:{bold:true,color:TEAL}},"","","",""],
   [{t:"  Energy jobs",o:{bold:true}},"20",{t:"20",o:{bold:true,color:GREEN}},{t:"20",o:{bold:true,color:GREEN}},{t:"0",o:{color:GREEN}}],
   ["  Energy deprivation","20","13","6","3"],
   ["  PM2.5 mortality","20","9","4","1"],
   [{t:"  TOTAL",o:{bold:true}},{t:"60",o:{bold:true}},{t:"42",o:{bold:true}},{t:"30",o:{bold:true}},{t:"4",o:{bold:true}}]],
  [4.4,1.6,2.2,2.0,1.9],{t:"42 / 60",c:GOLD},
  "Nine regions plus World, primary measure per family. The SCI-vetted 1.5°C column runs on seven scenarios per arm and should not be quoted as the strongest result — it scores highest because it is smallest.");

section("Part four","Why — mechanism and real world","Why these results, given how the outcomes are built and how energy systems work.");

table("Mechanism — jobs","Where the employment gap comes from, and what High-CMT wins",
  ["Technology group","High-CMT","High-RE","Gap","Reading"],
  [[{t:"Renewables",o:{bold:true}},"322.1","693.4",{t:"+371.3",o:{bold:true,color:GREEN}},
    "The entire result — wind and solar construction, manufacturing and O&M"],
   [{t:"Bioenergy",o:{bold:true}},"49.9","5.9",{t:"−44.1",o:{bold:true,color:CMT}},
    "The largest single loss. BECCS-heavy CMT carries a substantial biomass supply workforce"],
   ["Fossil","38.1","12.8",{t:"−25.3",o:{color:CMT}},
    "Fossil CCS extends the fuel supply chain — the mine and the well keep running"],
   ["Nuclear","25.6","8.2",{t:"−17.4",o:{color:CMT}},
    "High-CMT leans on firm low-carbon capacity that High-RE displaces"],
   [{t:"NET",o:{bold:true}},{t:"290.97",o:{bold:true}},{t:"686.78",o:{bold:true}},{t:"+395.8",o:{bold:true,color:GREEN}},
    {t:"Renewables outweigh all three losses combined by 4.4 to 1",o:{bold:true}}]],
  [2.4,1.5,1.5,1.5,5.2],{t:"1.5°C, WORLD",c:TEAL},
  "Job-years per 1,000 people, cumulative 2020–2100. THE MECHANISM IS UNAMBIGUOUS: High-CMT wins bioenergy, fossil and nuclear employment combined (−86.8) and still loses by 396, because the renewable build is 2.2 times larger. This also answers the obvious objection — the result is NOT an artefact of counting fossil job destruction, because High-CMT retains more fossil, nuclear AND biomass work and loses anyway.");

bullets("Why — World","The jobs mechanism is an engineering fact, not a modelling artefact",
  ["LABOUR INTENSITY PER UNIT OF CAPACITY IS HIGHER FOR BUILDING THAN FOR FUELLING. A gas turbine with capture needs a fuel supply chain and a handful of operators. An equivalent wind or solar build needs factories, foundations, cabling and installation crews. Over 2020–2100 the renewable term is 693 job-years per 1,000 against 322 — 2.2 times larger — and that single term carries the entire result.",
   "ENGINEERED CARBON MANAGEMENT IS CAPITAL-INTENSIVE AND LABOUR-SPARING BY DESIGN. Its appeal is that it lets the existing energy system keep running with a bolt-on. That is exactly why it employs fewer people: the point of the technology is to avoid rebuilding the system, and rebuilding the system is where the work is.",
   "THE THREE THINGS HIGH-CMT WINS ARE REAL AND WORTH NAMING. Bioenergy (+44), fossil (+25) and nuclear (+17) all favour it. These are not trivial jobs — biomass supply is rural, nuclear is high-skill and unionised, fossil extraction is regionally concentrated. A just-transition argument has to engage with all three, and the paper should say so rather than reporting only the net.",
   "THE REAL-WORLD EVIDENCE POINTS THE SAME WAY. Observed employment per GW installed in solar PV and onshore wind exceeds that of new thermal capacity in essentially every published national accounting, and the gap is widest in the build phase. The models are reproducing something already visible in labour statistics."],
  {t:"WORLD",c:GOLD});

bullets("Why — World","Why deprivation moves, and why the World cell no longer clears zero",
  ["THE MECHANISM IS DELIVERED FINAL ENERGY. The gap is max(0, threshold − actual) applied per sector and truncated at zero, so a pathway closes it by delivering more usable energy to the sectors that fall short. High-RE closes the World gap from 18.67 to 12.98 GJ per capita — a 30% reduction, the largest proportional wellbeing effect in the study after jobs.",
   "BUT THE INTERVAL NOW TOUCHES ZERO: [−1.21, +12.48] at 1.5°C and [−0.38, +8.05] at 2°C. The point estimate is large and the direction consistent; what is missing is precision. The reason is visible in the regional table — Africa moves by 22 GJ per capita with an interval of [−9.4, +46.3], because African deprivation levels vary enormously between models.",
   "SO THE WORLD NUMBER IS AN AVERAGE OF REGIONS THAT DISAGREE. Latin America (−23.1, significant), Europe (−3.1, significant), India+ (−0.6, significant) and China+ (−0.3, significant) push one way; the Middle East (+1.2, significant) and Rest of Asia at 2°C (+3.0, significant) push the other. That is a genuine split, not a weak global effect.",
   "WHAT THIS MEANS FOR THE CLAIM. Report deprivation as a large, directionally consistent REGIONAL result that does not resolve at World. Four regions support it significantly; two oppose it significantly. That is more honest and more useful than a global average with a wide interval."],
  {t:"WORLD",c:GOLD});

bullets("Why — World","Why mortality is finally reportable, and why Europe carries it",
  ["THE INPUT RULE IS WHAT CHANGED. Requiring all five PM2.5 precursors to be reported directly at R10 removes the ammonia asymmetry at source. In the previous round REMIND reported almost no agricultural ammonia while other models reported 6–12%, flattering High-RE by roughly 9%. Scenarios that cannot support the calculation are now dropped rather than filled.",
   "EUROPE IS THE RESULT. 4.61 million cumulative deaths avoided at 1.5°C and 3.76 million at 2°C, both significant, on a base of about 15 million — a 29% reduction, the largest proportional mortality effect of any region. Europe also has the densest population living closest to its combustion sources, so a given reduction in primary PM and precursors buys more avoided exposure there than almost anywhere.",
   "THE WORLD RESULT IS SIGNIFICANT AT 1.5°C AND NOT AT 2°C — 8.69 million avoided [+0.97, +46.90] against −3.03 [−19.4, +29.8]. That asymmetry is consistent with the mechanism: at higher ambition the two arms differ more in how much combustion remains, so the air-quality consequence is larger.",
   "FOURTEEN OF TWENTY CELLS STILL STRADDLE ZERO. Mortality remains the least precise outcome, because it runs on the smallest sample and because regional PM2.5 is dominated by sources — residential solid fuel, dust, agriculture — that the power-sector mix does not control. Report the Europe result and the World 1.5°C result; do not claim a universal air-quality co-benefit."],
  {t:"WORLD",c:GOLD});


section("Part five","Why — region by region","Each region's own mechanism, and its real-world reading.");

function R2(name,verdict,vc,jobs,dep,hea,extra,mech,real,note){
  const st=[["Jobs 1.5°C / 2°C",jobs[0],jobs[1]],
            ["Deprivation gap, GJ/cap",dep[0],dep[1]],
            ["Mortality, million deaths",hea[0],hea[1]]].concat(extra);
  region(name,verdict,vc,st,mech,real,note);
}

R2("India+ — the largest employment prize in the study","3 / 3",GREEN,
  ["268.0 → 1000.6 · 209.7 → 664.7",GREEN],["1.09 → 0.53 · 1.13 → 0.45",GREEN],
  ["109.1 v 104.6 · 114.3 v 116.3",null],
  [["Jobs multiple","3.7× · 3.2×",GREEN],["Deprivation, both levels","significant",GREEN],
   ["Baseline PM2.5 burden","109–114 m deaths",RED],["Mortality 1.5°C","+4.5 m, not significant"]],
  "India+ shows the second-largest absolute jobs gap of any region (+733 job-years per 1,000 at 1.5°C) on top of an already-high High-CMT base — the renewable build is 3.7 times larger, not merely additive. Its decent-living threshold is the lowest anywhere at 10 GJ/cap, so the small absolute deprivation gap (0.53 GJ/cap under High-RE) still clears the interval at BOTH ambition levels, which only India+ and Latin America manage.",
  "India's power build is genuinely greenfield: there is no large stranded workforce to displace and the solar manufacturing base is being built now rather than defended. The engineered-CMT alternative here means coal or gas with capture, which locks in both the mine and the import bill. India+ also carries the largest absolute PM2.5 burden of any region at 109–114 million cumulative deaths, so even a non-significant 4.5 million avoided at 1.5°C is a large number in human terms.",
  "The mortality cells do not clear the interval at either level and the sign flips between them, so India+ supports the jobs and deprivation claims but not an air-quality claim.");

R2("Europe — the only region where all three outcomes hold","3 / 3",GREEN,
  ["91.5 → 154.5 · 57.0 → 114.9",GREEN],["5.95 → 2.80 · 3.32 → 2.66",GREEN],
  ["15.8 v 11.2 · 15.0 v 11.2",GREEN],
  [["Mortality reduction","29% · 25%",GREEN],["Mortality significance","BOTH levels",GREEN],
   ["Jobs, smallest absolute gap","+63 · +58"],["Deprivation 2°C","not significant"]],
  "Europe has the SMALLEST absolute jobs gap of any region (+63 job-years per 1,000) because its build is small relative to its incumbent system — and it still clears the interval at both levels. Its distinguishing result is mortality: 4.61 and 3.76 million cumulative deaths avoided on a base of about 15 million, a 29% and 25% reduction. That is the largest proportional air-quality effect anywhere and the only mortality result significant at both ambition levels.",
  "Europe's population lives densely and close to its combustion sources, so a given reduction in primary PM and precursors buys more avoided exposure than almost anywhere else. It is also the region where the reporting-complete rule bites least — European models report the full precursor set — so the mortality estimate rests on comparatively solid input data. Europe is the region to lead with when the audience is air-quality policy.",
  "Deprivation clears at 1.5°C (−3.14 GJ/cap) but not at 2°C (−0.66). Europe is the strongest all-round case in the study and the natural anchor for the paper's three-outcome claim.");

R2("Africa — the largest jobs gap, and the widest uncertainty","1 / 3",GOLD,
  ["225.8 → 1057.6 · 167.4 → 705.8",GREEN],["67.3 → 45.2 · 65.6 → 40.8",null],
  ["75.4 v 78.5 · 76.3 v 78.7",RED],
  [["Jobs multiple","4.7× · 4.2×",GREEN],["Deprivation interval 1.5°C","[−9.4, +46.3]",RED],
   ["Baseline deprivation gap","65–67 GJ/cap",RED],["Mortality 2°C","−2.3 m, significant",RED]],
  "Africa has the largest jobs gap in the study by a wide margin: +832 job-years per 1,000 at 1.5°C, a 4.7-fold multiple, because there is almost nothing to retire and everything to build. Its deprivation baseline is also by far the worst — a 65–67 GJ/cap shortfall against a 17 GJ/cap threshold — and High-RE closes 22–25 GJ/cap of it. But the interval is [−9.4, +46.3]: the point estimate is enormous and the uncertainty is larger still, because African deprivation levels differ enormously between models.",
  "Africa is where the wellbeing stakes are highest and the modelling agreement is weakest. The jobs result is unambiguous and should be led with. The deprivation result should be described as a large central estimate that the ensemble cannot yet pin down — which is itself a finding about how poorly AR6 models agree on African energy futures. The mortality cell goes mildly against High-RE, which is unsurprising: African PM2.5 is dominated by residential solid fuel and dust, not the power mix.",
  "Africa is the clearest illustration of why the paper reports intervals rather than point estimates. On the medians alone it would look like the strongest deprivation case in the study.");

R2("Latin America — the most complete deprivation result","2 / 3",GREEN,
  ["271.6 → 492.4 · 166.3 → 360.0",GREEN],["62.9 → 39.8 · 51.0 → 39.6",GREEN],
  ["15.3 v 15.7 · 14.0 v 15.5",null],
  [["Deprivation, both levels","significant",GREEN],["Gap closed 1.5°C","−23.1 GJ/cap",GREEN],
   ["Baseline deprivation gap","51–63 GJ/cap",RED],["Land CDR","now excluded from CMT",TEAL]],
  "Latin America closes the largest deprivation gap that actually clears the interval: −23.1 GJ/cap at 1.5°C and −11.4 at 2°C, significant at both. Unlike Africa its models agree, so the result is both large and precise. Jobs is solid (+221 and +194) though not among the largest multiples, because Latin America already has a substantial renewable base and the incremental build is smaller in relative terms.",
  "This region is the one most changed by the revamp. Under the old Total-CDR axis, Latin American High-CMT was 73% LAND-BASED removal — afforestation and soil carbon — so the comparison was scoring an intervention the energy-outcome set cannot see. Excluding land makes the Latin America contrast a genuine energy-system comparison for the first time, and the deprivation result is correspondingly more interpretable.",
  "Mortality is mildly negative at both levels and clears at neither. Latin America's PM2.5 base is small (14–16 million) so there is little room for the power mix to move it.");

R2("Middle East — jobs yes, energy access no","1 / 3",GOLD,
  ["294.2 → 851.0 · 127.5 → 538.4",GREEN],["1.51 → 2.73 · 0.80 → 2.25",RED],
  ["45.8 v 46.2 · 43.8 v 46.7",null],
  [["Jobs multiple","2.9× · 4.2×",GREEN],["Deprivation, both levels","significantly AGAINST",RED],
   ["Gap widened","+1.23 · +1.44",RED],["Baseline PM2.5","44–47 m deaths"]],
  "The Middle East has the second-largest jobs multiple in the study (4.2× at 2°C) and is the ONLY region where deprivation reverses significantly at both ambition levels — the gap WIDENS from 1.51 to 2.73 GJ/cap at 1.5°C. Those two facts sit together because the region starts from an unusually low renewable share and an unusually high per-capita energy base: building renewables creates a great deal of work while the delivered-energy trajectory under High-RE falls short of what the carbon-management pathways sustain.",
  "For the Gulf, engineered carbon management is not a climate policy bolted onto the economy — it IS the economy. Fossil CCS lets hydrocarbon production and export continue while the emissions are captured. High-RE means abandoning the export base, and the models reflect the domestic energy consequence of doing so. This is the clearest case in the study where the wellbeing comparison and the political-economy reality point in genuinely different directions.",
  "The Middle East is the strongest single counter-example to a universal High-RE wellbeing claim, and the paper is stronger for reporting it prominently rather than burying it.");

R2("China+ — solid on both, quiet on health","2 / 3",GREEN,
  ["209.5 → 333.7 · 157.7 → 248.7",GREEN],["1.14 → 0.80 · 0.81 → 0.68",GREEN],
  ["78.6 v 77.4 · 80.3 v 78.7",null],
  [["Jobs multiple","1.6× · 1.6×",null],["Deprivation 1.5°C","significant",GREEN],
   ["Deprivation 2°C","not significant"],["Baseline PM2.5","79–80 m deaths",RED]],
  "China+ shows the SMALLEST jobs multiple of any region at 1.6× — but on a large base, so the absolute gap (+124 and +91 job-years per 1,000) still clears the interval comfortably at both levels. The modest multiple reflects that China's High-CMT pathways already involve a very large build; the two arms differ less in whether they construct than in what they construct.",
  "China+ carries the second-largest absolute PM2.5 burden in the study (79–80 million cumulative deaths) and the mortality difference between arms is small and not significant in either direction. The plausible reason is that Chinese PM2.5 is heavily influenced by agricultural ammonium nitrate and industrial process emissions that neither pathway addresses — the power-sector mix is not the binding constraint on Chinese air quality within this horizon.",
  "Deprivation clears at 1.5°C (−0.34 GJ/cap) but not at 2°C. China+ is a reliable contributor to the headline without being the story anywhere.");

R2("Rest of Asia — the largest multiple, and a 2°C reversal","1 / 3",GOLD,
  ["170.7 → 836.8 · 98.3 → 540.3",GREEN],["10.65 → 8.33 · 3.83 → 6.84",RED],
  ["55.5 v 56.1 · 58.4 v 57.9",null],
  [["Jobs multiple","4.9× · 5.5×",GREEN],["Largest multiple in the study","5.5× at 2°C",GREEN],
   ["Deprivation 1.5°C","−2.32, not significant"],["Deprivation 2°C","+3.02, significant",RED]],
  "Rest of Asia has the LARGEST jobs multiple anywhere — 5.5× at 2°C, from 98 to 540 job-years per 1,000 — for the same reason as Africa and India+: a very large remaining build against a small incumbent fleet. Deprivation splits by ambition: mildly favourable at 1.5°C (−2.32, not significant) and significantly against at 2°C (+3.02). That split is the single most awkward cell in the regional table.",
  "The region aggregates economies at very different stages — Indonesia, Vietnam, Thailand, the Philippines — so a single R10 label conceals more heterogeneity here than almost anywhere. The 2°C deprivation reversal is most plausibly a composition effect within the region rather than a coherent mechanism, and the paper should say so rather than construct a story for it.",
  "The jobs result is among the strongest in the study; the deprivation result should be reported as ambiguous rather than as a reversal.");

R2("North America — jobs hold, the other two do not","1 / 3",GOLD,
  ["190.8 → 344.6 · 106.5 → 209.4",GREEN],["7.62 → 8.51 · 7.07 → 8.05",RED],
  ["7.7 v 8.9 · 6.8 v 8.9",RED],
  [["Jobs multiple","1.8× · 2.0×",GREEN],["Deprivation","widens, not significant",RED],
   ["Mortality","−1.2 · −2.1, not significant",RED],["Smallest PM2.5 base","6.8–8.9 m deaths"]],
  "North America's jobs gap is solid and significant at both levels (+154 and +103) on a 1.8–2.0× multiple. Both other outcomes go mildly against High-RE and neither clears the interval: the deprivation gap widens by about 0.9 GJ/cap, and mortality is 1.2–2.1 million higher. The mortality base is the smallest of any region at 6.8–8.9 million, so there is very little room for the power mix to move it.",
  "North America has the highest incumbent nuclear capacity of any region, and the jobs decomposition shows nuclear employment favouring High-CMT globally by 17 job-years per 1,000 — a penalty concentrated in exactly this region. The deprivation reading is less interpretable: at 37 GJ/cap North America has the highest decent-living threshold anywhere, so its residual gap is a measurement of how far a rich region sits from a demanding benchmark rather than of energy poverty in the usual sense.",
  "Under the previous design North America's mortality cell showed complete separation and was reported as unresolvable. The reporting-complete rule has replaced that artefact with an ordinary non-significant difference, which is a real improvement.");

R2("Reforming economies — strong jobs, and a rare health signal","2 / 3",GREEN,
  ["503.4 → 879.0 · 297.8 → 600.9",GREEN],["1.87 → 1.96 · 1.15 → 1.39",null],
  ["14.1 v 13.1 · 14.2 v 13.3",GREEN],
  [["Highest jobs base anywhere","503 · 298"],["Jobs, both levels","significant",GREEN],
   ["Mortality 1.5°C","+0.98 m, significant",GREEN],["Deprivation","flat, not significant"]],
  "Reforming economies carries the highest High-CMT jobs base of any region (503 job-years per 1,000 at 1.5°C) — a reflection of its very large incumbent energy workforce — and High-RE still adds +376 on top of it. It is also one of only three regions with a significant mortality result favouring High-RE (+0.98 million avoided at 1.5°C), alongside Europe and Pacific OECD.",
  "Russia and Central Asia hold the largest fossil fleet in the study, so this is the region where a transition looks most like a net industrial disruption. That the jobs result still clears comfortably — and that it does so while High-CMT retains more fossil, nuclear and biomass employment — is the strongest available evidence that the renewable build genuinely outweighs what it displaces rather than merely accounting it away.",
  "Deprivation is flat (+0.09 and +0.24, neither significant). Under the previous design this region's mortality cell was another unresolvable complete-separation case; it is now an ordinary significant result at 1.5°C.");

R2("Pacific OECD — excluded from the regional rows","EXCLUDED",RED,
  ["138.1 → 127.8 · 76.9 → 94.9",RED],["5.74 → 6.50 · 3.88 → 5.15",RED],
  ["5.4 v 3.6 · 5.6 v 3.6",GREEN],
  [["Jobs 1.5°C","−10.4, not significant",RED],["Jobs 2°C","+18.1, not significant",RED],
   ["Deprivation 2°C","+1.27, significant against",RED],["Mortality","+1.8 · +1.9, both significant",GREEN]],
  "Pacific OECD is the only region where the jobs result fails at BOTH ambition levels — indeed it goes NEGATIVE at 1.5°C (−10.4 job-years per 1,000). It is also the only region whose mortality cells are significant while its jobs cells are not, which is the reverse of the pattern everywhere else. Deprivation goes significantly against High-RE at 2°C.",
  "The region is small (Japan, Korea, Australia, New Zealand), highly developed, and already substantially built out, so the marginal renewable build that distinguishes the two arms elsewhere is much smaller here. As in the previous design, the classification does not describe local behaviour well enough to support a regional reading.",
  "Retained inside the World aggregate — dropping it there would change what 'World' means — and excluded from the nine regional rows. This is a display decision, not a data exclusion: nothing is discarded.");

section("Part six","But is it real?",
  "Model composition, the land-based CDR boundary, and the scenarios that never reported renewables.");

bullets("The confound","Better than it was, and still the binding constraint",
  ["THE ARMS ARE NO LONGER DISJOINT BY MODEL. REMIND is 71% of High-RE and 1% of High-CMT — down from 85% and 1%. More importantly MESSAGEix now holds 44 High-CMT AND 37 High-RE scenarios, where previously it held 118 and 4. Eight of ten families hold both arms. The engineered-CMT axis and the balanced portfolio rule together did this.",
   "IT STILL MATTERS. A single family supplying seven in ten scenarios of one arm means any pooled difference remains partly a modelling-framework contrast. The improvement lowers the price of the claim; it does not remove the need to pay it.",
   "THE WITHIN-MODEL EVIDENCE NOW SPLITS BY AMBITION. On jobs at 2°C, six model families contribute and ALL SIX show High-RE higher — the strongest within-model confirmation in the study. At 1.5°C only three families qualify and the median effect is mixed, so the 1.5°C jobs cells rest more heavily on the pooled comparison than the 2°C cells do.",
   "ON DEPRIVATION THE WITHIN-MODEL MEDIAN OFTEN DISAGREES WITH THE POOLED DIRECTION, as it did in the previous design. This is the outcome most exposed to which models populate which arm, and the reason its claim is stated regionally rather than globally."],
  {t:"THE CONFOUND",c:RED},
  "Source: century_outcome_within_model_no_land_engineered_cmt.csv. The n_models column is the count of families holding both arms in that cell — 3 at 1.5°C and 5–6 at 2°C.");

table("Is it real?","What survives, and what the reader should take on trust",
  ["","Pooled result","Within-model support","Verdict"],
  [[{t:"Energy jobs",o:{bold:true}},{t:"20/20, all 20 significant",o:{bold:true,color:GREEN}},
    {t:"6 of 6 families at 2°C; mixed at 1.5°C",o:{color:GREEN}},{t:"a result",o:{bold:true,color:GREEN}}],
   [{t:"Energy deprivation",o:{bold:true}},"13/20, 6 significant, 3 against",
    {t:"within-model median often disagrees",o:{color:RED}},
    {t:"a regional result, not a global one",o:{color:GOLD}}],
   [{t:"PM2.5 mortality",o:{bold:true}},"9/20, 4 significant, 1 against",
    "1 family at 1.5°C, 3 at 2°C — thin",
    {t:"Europe holds; World 1.5°C holds; no universal claim",o:{color:GOLD}}]],
  [2.8,3.4,3.6,2.3],{t:"VERDICT",c:GOLD},
  "The asymmetry is the point. The same test applied identically to all three families clears jobs, qualifies deprivation and limits mortality to two specific claims. That cannot be dismissed as an unfair standard, and it is what makes the jobs result worth trusting.");

bullets("Sensitivity — the land boundary","What happens if land-based removal goes back into the CMT axis",
  ["THE QUESTION. The headline design defines carbon management as ENGINEERED removal — Novel CDR plus fossil and industrial CCS — and excludes afforestation, reforestation and soil carbon. That is a boundary choice, and a reviewer will ask what it is doing. The test re-runs the entire classification with land-based removal added to the CMT axis and scores both label sets on IDENTICAL outcome data, so only the labels differ.",
   "THE LABELS BARELY MOVE. Of 530 scenarios classified under both axes, NOT ONE switches arms — 100% keep the same label. The two axes correlate 0.898, and land-based removal is 28% of the with-land axis. The boundary is a smaller lever than its prominence in the literature suggests.",
   "AND WHERE IT MOVES THE RESULT, IT MOVES IT TOWARDS HIGH-RE. Including land makes the jobs advantage LARGER, not smaller: World 1.5°C goes from +396 to +456 job-years per 1,000, because land-heavy carbon-management scenarios carry fewer energy jobs than engineered-CDR ones. Mortality also strengthens, from 8.69 to 11.11 million avoided at World 1.5°C, significant on either axis. EXCLUDING LAND IS THEREFORE THE CONSERVATIVE CHOICE — the published design reports the smaller advantage.",
   "SO WHY EXCLUDE IT ANYWAY? Two reasons, and neither is about the size of the effect. First, land-based removal is a different intervention whose real costs — land competition, food prices, tenure, biodiversity — are precisely what this outcome set CANNOT see, so scoring it here would flatter the comparison. Second, model composition is worse on the land axis: GEM drops from 25 High-CMT scenarios to 0, COFFEE from 12 to 0, and the families holding both arms fall from eight to seven. A cleaner contrast on a better-populated sample is worth giving up a larger point estimate."],
  {t:"LAND IN / OUT",c:TEAL},
  "Source: W12_land_sensitivity.R and W13_zeros_and_land_mortality.R. Both label sets are built with the published classification code and the published normalisation, and self-check against the published labels at 99.9%.");

img("Sensitivity — the land boundary","Every cell, both axes, on the same outcome data",FIG+"F3_land.png",
  "Grey is the published engineered axis; the arrow head is engineered plus land-based removal.",
  "Positive favours High-RE. The arrows are short, they mostly point the same way, and where they lengthen they lengthen in High-RE's favour. No cell changes sign in jobs; the land axis is not holding the result up.",
  {t:"FIGURE F3",c:TEAL},0.595);

bullets("Sensitivity — the zeros","A quarter of the 1.5°C High-CMT arm never reported renewables at all",
  ["THE PROBLEM. The classification pivots renewable capacity with a zero fill, so a scenario that never reports Renewable Capacity is scored as deploying none. That puts it at the bottom of the RE distribution and makes it eligible for High-CMT. At 1.5°C, 16 of the 64 High-CMT scenarios — a quarter of the arm — sit at exactly zero renewables; 29 of 239 at 2°C. There are NONE in the High-RE arm, by construction.",
   "THEY ARE MISSING DATA, NOT ZEROS. Every one of the 50 was checked against the cumulative deployment file for a Renewable Capacity ROW. All 50 have no row at all. Not one is a reported zero. They are concentrated in GCAM (33 of 50), which is a reporting convention rather than a modelling result — and the SCI-vetted sample already excludes every one of them.",
   "AND THEY ARE NOT NEUTRAL. Their median World net energy employment is −2.3 job-years per 1,000, against 308.4 for the High-CMT scenarios that do report. They are dragging the High-CMT arm down on the outcome, on the strength of an axis value that was invented by the fill.",
   "WHAT DROPPING THEM DOES. Jobs weakens slightly and still clears easily: World 1.5°C +395.8 → +378.4. DEPRIVATION GOES THE OTHER WAY AND BECOMES SIGNIFICANT AT BOTH LEVELS — 1.5°C from −5.69 [−1.14, +12.39] to −8.45 [+2.51, +17.33], and 2°C from −4.31 to −6.87 [+0.60, +9.11]. That is a headline change, so it is presented here as a labelled sensitivity and the decision on whether to make it the default is a reporting choice, not a technical one."],
  {t:"THE ZEROS",c:RED},
  "Source: W13_zeros_and_land_mortality.R. Whichever way this is resolved, the direction of every World cell is unchanged — dropping the zeros makes the deprivation result stronger, never weaker.");

img("Sensitivity — the zeros","What the non-reporting scenarios do to the High-CMT arm",FIG+"F5_zeros.png",
  "World net energy employment for the High-CMT arm only, split by whether the scenario reported renewable capacity.",
  "The non-reporting group sits at roughly zero net energy employment while the reporting group sits around 300 job-years per 1,000. The zero fill is not a harmless default: it recruits scenarios into the comparison arm on an axis value they never supplied.",
  {t:"FIGURE F5",c:RED},0.5);

section("Part seven","What we can claim","And what has to be said alongside it.");

bullets("The claim","What renewables-led mitigation delivers, and how sure we are",
  ["ONE — WORK. At matched climate ambition, renewables-led mitigation employs substantially more people in energy: 291 against 687 job-years per 1,000 at World 1.5°C, a difference of +396 [+285, +508], and +293 [+147, +411] at 2°C. Better in ALL 20 region × ambition cells and significant in all 20 — the only outcome of which that is true. The mechanism is the renewable build, which outweighs High-CMT's bioenergy, fossil and nuclear employment combined by 4.4 to 1.",
   "TWO — ENERGY ACCESS, REGIONALLY. Renewables-led pathways close the decent-living energy gap significantly in Latin America (−23.1 GJ per capita), Europe (−3.1), India+ (−0.6) and China+ (−0.3), and widen it significantly in the Middle East (+1.2). The World aggregate is −5.69 GJ per capita but its interval touches zero, so this is a regional result rather than a global one.",
   "THREE — AIR QUALITY, IN EUROPE. Renewables-led pathways avoid 4.61 million cumulative PM2.5 deaths in Europe at 1.5°C and 3.76 million at 2°C, both significant — a 29% reduction, the largest proportional effect anywhere. At World the 1.5°C cell is significant (8.69 million avoided) and the 2°C cell is not. Fourteen of twenty cells straddle zero, so there is no universal air-quality co-benefit to claim.",
   "TAKEN TOGETHER: the choice between engineered carbon management and renewables matters decisively for employment, regionally for energy access, and in specific places for air quality. That asymmetry is the paper's answer, and it is more useful than three uniform ticks would have been."],
  {t:"CLAIM",c:GOLD});

bullets("Limitations","Stated plainly, because they are load-bearing",
  ["MODEL COMPOSITION REMAINS THE BINDING CONSTRAINT, though materially better than before. REMIND supplies 71% of High-RE against 1% of High-CMT. The within-model check supports jobs at 2°C strongly, jobs at 1.5°C weakly, and deprivation not at all.",
   "THE WORLD AGGREGATION DEFECT IS FIXED, ported into the master, and now the default. The master built World by summing outcomes WITHIN each deployment-variable group, so a scenario's World total inherited that variable's regional coverage. World outcomes are now aggregated only when all ten R10 values are present, gated per outcome. It moves World jobs at 2°C from 461.4 to 466.0 and leaves 1.5°C unchanged; regional cells are untouched by construction.",
   "FIFTY SCENARIOS ENTER THE HIGH-CMT ARM ON A RENEWABLES VALUE THEY NEVER REPORTED. The zero fill treats a missing Renewable Capacity row as zero deployment. All 50 are missing rows, not reported zeros, and 16 of them are a quarter of the 1.5°C High-CMT arm. Retaining them is the conservative choice for jobs and the ANTI-conservative one for deprivation, which becomes significant at both levels without them.",
   "MORTALITY RUNS ON A RESTRICTED SAMPLE BY DESIGN — 422 of 643 classified targets. The restriction is the right call, but it means the mortality arms are smaller than the jobs and deprivation arms and the comparison is correspondingly less precise.",
   "THE DEPRIVATION MEASURE TRUNCATES AT ZERO, so it responds only to sectors where a region falls short — not always the household sector. It is a regional aggregate and cannot speak to who inside a region is deprived.",
   "EXCLUDING LAND-BASED REMOVAL SHARPENS THE COMPARISON BUT NARROWS IT. The paper now compares two energy-system strategies cleanly, and says nothing about land-based removal, which is a large part of many real mitigation portfolios and carries its own distributional consequences."],
  {t:"LIMITS",c:RED});

bullets("What is open","Next steps, in priority order",
  ["DECIDE HOW TO REPORT THE ZERO-RENEWABLE SCENARIOS. The 50 scenarios that never reported renewable capacity are currently retained with a zero fill, which is the published behaviour. Dropping them makes the World deprivation result significant at both ambition levels. This is the one open item that changes a headline, and it is a reporting decision rather than a technical one.",
   "REPORT WITHIN-MODEL RESULTS ALONGSIDE POOLED ONES in the paper, not only the SI. The 2°C jobs cell — six of six families agreeing — is the most persuasive number in the study and it is currently buried.",
   "CONSIDER A LAND-BASED CDR COMPANION ANALYSIS. Excluding land was right for this comparison and it is the conservative choice, but the excluded pathways are not uninteresting; they are a different paper with a different outcome set — land competition, food prices and tenure rather than jobs and air quality.",
   "RE_SPEC DEFINITION SENSITIVITY — whether the renewables axis should include nuclear or biomass. A reviewer will ask, and the answer is currently a principled argument without a table behind it.",
   "DONE SINCE THE LAST ROUND: the strict World aggregation is ported into the master and verified against the downstream implementation to machine precision; the land boundary is tested in both directions; and the zero-renewable scenarios are diagnosed rather than assumed."],
  {t:"OPEN",c:TEAL});

// Written into decks/ rather than the working directory, for the same reason the
// figures are: running this from the repo root should not leave files there.
const OUT = (process.env.COMPASS_DECK || "decks/COMPASS_Paper1_final_8.25.pptx");
require("fs").mkdirSync(require("path").dirname(OUT), {recursive:true});
p.writeFile({fileName:OUT}).then(()=>console.log("slides:",N,"->",OUT));
