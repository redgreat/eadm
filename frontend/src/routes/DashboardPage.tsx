import { createMemo, createResource, For, Show } from "solid-js";
import { getDashboardSummary } from "../lib/api/dashboard";

export default function DashboardPage() {
  const [summary] = createResource(getDashboardSummary);
  const metrics = createMemo(() => {
    const cards = summary()?.data.cards;
    return [
      { label: "健康数据", value: cards?.health ?? "--", caption: "本周健康汇总" },
      { label: "轨迹位置", value: cards?.location ?? "--", caption: "本周定位汇总" },
      { label: "财务收入", value: cards?.financeIncome ?? "--", caption: "本周收入汇总" },
      { label: "财务支出", value: cards?.financeExpense ?? "--", caption: "本周支出汇总" }
    ];
  });

  return (
    <div class="space-y-6">
      <section>
        <h2 class="text-2xl font-semibold tracking-tight">仪表盘</h2>
        <p class="mt-1 text-sm text-slate-500">首页已接入新的 `/api/dashboard/summary` 接口。</p>
      </section>

      <Show when={summary()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {summary()?.message || "首页数据加载失败"}
        </div>
      </Show>

      <section class="grid gap-4 md:grid-cols-2 xl:grid-cols-4">
        <For each={metrics()}>
          {(metric) => (
            <article class="rounded-lg border border-slate-200 bg-white p-5 shadow-sm">
              <div class="text-sm text-slate-500">{metric.label}</div>
              <div class="mt-3 text-3xl font-semibold">{metric.value}</div>
              <div class="mt-2 text-xs text-slate-400">{metric.caption}</div>
            </article>
          )}
        </For>
      </section>

      <section class="grid gap-4 xl:grid-cols-2">
        <TrendPanel
          title="轨迹趋势"
          labels={summary()?.data.locationTrend.labels ?? []}
          values={summary()?.data.locationTrend.values ?? []}
        />
        <TrendPanel
          title="财务趋势"
          labels={summary()?.data.financeTrend.labels ?? []}
          values={summary()?.data.financeTrend.income ?? []}
          secondaryValues={summary()?.data.financeTrend.expense ?? []}
        />
      </section>
    </div>
  );
}

function TrendPanel(props: { title: string; labels: string[]; values: string[]; secondaryValues?: string[] }) {
  return (
    <article class="rounded-lg border border-slate-200 bg-white p-5 shadow-sm">
      <h3 class="text-base font-semibold">{props.title}</h3>
      <div class="mt-4 space-y-3">
        <Show when={props.labels.length > 0} fallback={<div class="text-sm text-slate-400">暂无趋势数据</div>}>
          <For each={props.labels}>
            {(label, index) => (
              <div class="grid grid-cols-[64px_1fr] items-center gap-3 text-sm">
                <div class="text-slate-500">{label}</div>
                <div class="flex items-center gap-3">
                  <div class="h-2 flex-1 rounded-full bg-slate-100">
                    <div class="h-2 rounded-full bg-slate-950" style={{ width: barWidth(props.values[index()]) }} />
                  </div>
                  <div class="w-20 text-right font-medium">{props.values[index()] ?? "0"}</div>
                  <Show when={props.secondaryValues}>
                    <div class="w-20 text-right text-slate-500">{props.secondaryValues?.[index()] ?? "0"}</div>
                  </Show>
                </div>
              </div>
            )}
          </For>
        </Show>
      </div>
    </article>
  );
}

function barWidth(value: string | undefined) {
  const parsed = Number(value ?? 0);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    return "4%";
  }
  return `${Math.min(100, Math.max(8, parsed))}%`;
}
