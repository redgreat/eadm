import { useLocation } from "@solidjs/router";
import type { ParentProps } from "solid-js";
import { Show } from "solid-js";
import AdminLayout from "../components/layout/AdminLayout";

export default function App(props: ParentProps) {
  const location = useLocation();

  return (
    <Show when={location.pathname !== "/login"} fallback={props.children}>
      <AdminLayout>{props.children}</AdminLayout>
    </Show>
  );
}
