import { A } from "@solidjs/router";

export default function NotFoundPage() {
  return (
    <div class="rounded-lg border border-slate-200 bg-white p-8 text-center shadow-sm">
      <h2 class="text-xl font-semibold">页面不存在</h2>
      <p class="mt-2 text-sm text-slate-500">当前页面还没有迁移到 SolidJS。</p>
      <A class="mt-4 inline-flex text-sm font-medium text-slate-950 underline" href="/">
        返回仪表盘
      </A>
    </div>
  );
}
