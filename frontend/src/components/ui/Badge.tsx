import type { JSX } from "solid-js";

export default function Badge(props: { tone?: "green" | "red" | "slate" | "blue" | "amber"; children: JSX.Element }) {
  const tone = () => props.tone ?? "slate";
  const classes = () => ({
    green: "bg-emerald-50 text-emerald-700",
    red: "bg-rose-50 text-rose-700",
    slate: "bg-slate-100 text-slate-600",
    blue: "bg-sky-50 text-sky-700",
    amber: "bg-amber-50 text-amber-700"
  })[tone()];

  return <span class={`rounded-full px-2 py-1 text-xs font-medium ${classes()}`}>{props.children}</span>;
}
