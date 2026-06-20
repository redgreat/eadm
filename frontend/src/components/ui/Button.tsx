import type { JSX } from "solid-js";
import { splitProps } from "solid-js";
import { cn } from "../../lib/cn";

type ButtonProps = JSX.ButtonHTMLAttributes<HTMLButtonElement> & {
  variant?: "primary" | "secondary" | "ghost";
};

export default function Button(props: ButtonProps) {
  const [local, rest] = splitProps(props, ["class", "variant"]);
  const variant = () => local.variant ?? "primary";

  return (
    <button
      class={cn(
        "inline-flex h-10 items-center justify-center rounded-md px-4 text-sm font-medium transition-colors disabled:cursor-not-allowed disabled:opacity-60",
        variant() === "primary" && "bg-slate-950 text-white hover:bg-slate-800",
        variant() === "secondary" && "bg-slate-100 text-slate-950 hover:bg-slate-200",
        variant() === "ghost" && "text-slate-600 hover:bg-slate-100 hover:text-slate-950",
        local.class
      )}
      {...rest}
    />
  );
}
