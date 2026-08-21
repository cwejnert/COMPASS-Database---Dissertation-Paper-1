#!/usr/bin/env python3
"""Assemble brief.html from brief.head.html + brief.body.html, inlining every
figure as a data: URI (the artifact CSP blocks external hosts, and the page must
be self-contained). Placeholders in the body map to files here."""
import base64, os, sys

FIGS = {
    "__V1__":   "V1_scorecard.png",
    "__V2__":   "V2_world_forest.png",
    "__V3__":   "V3_robustness.png",
    "__V4__":   "V4_jobs_decomposition.png",
    "__V5__":   "V5_tradeoff.png",
    "__V6__":   "V6_mortality_variance.png",
    "__P4A__":  "P4a_jobtype_region.png",
    "__P4B__":  "P4b_fuel_region.png",
    "__P2B__":  "P2b_two_factor.png",
}

here = os.path.dirname(os.path.abspath(__file__))
head = open(os.path.join(here, "brief.head.html")).read()
body = open(os.path.join(here, "brief.body.html")).read()

missing = [f for f in FIGS.values() if not os.path.exists(os.path.join(here, f))]
if missing:
    sys.exit("missing figures: " + ", ".join(missing))

for key, fn in FIGS.items():
    if key not in body:
        print("  warning: placeholder %s unused" % key)
    with open(os.path.join(here, fn), "rb") as fh:
        uri = "data:image/png;base64," + base64.b64encode(fh.read()).decode()
    body = body.replace(key, uri)

left = [k for k in FIGS if k in body]
if left:
    sys.exit("unsubstituted placeholders: " + ", ".join(left))

out = os.path.join(here, "brief.html")
open(out, "w").write(head + body)
mb = os.path.getsize(out) / 1e6
print("wrote brief.html  %.2f MB  (artifact limit 16 MB)" % mb)
if mb > 15:
    sys.exit("too large for an artifact")
