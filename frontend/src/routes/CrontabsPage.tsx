import { createResource, createSignal, For, Show } from "solid-js";
import Button from "../components/ui/Button";
import { getCrontabs } from "../lib/api/crontabs";

export default function CrontabsPage() {
  const [keyword, setKeyword] = createSignal("");
  const [query, setQuery] = createSignal("");
  const [crontabs] = createResource(query, getCrontabs);

  const handleSearch = (event: SubmitEvent) => {
    event.preventDefault();
    setQuery(keyword());
  };

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">定时任务</h2>
          <p class="mt-1 text-sm text-slate-500">已接入新的 `/api/crontabs` 只读列表接口。</p>
        </div>
        <div class="rounded-md bg-white px-3 py-2 text-sm text-slate-500 shadow-sm">
          共 {crontabs()?.data.total ?? "--"} 个任务
        </div>
      </section>

      <form class="flex flex-wrap gap-2 rounded-lg border border-slate-200 bg-white p-4 shadow-sm" onSubmit={handleSearch}>
        <input
          value={keyword()}
          onInput={(event) => setKeyword(event.currentTarget.value)}
          placeholder="按任务名搜索"
          class="h-10 min-w-64 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950"
        />
        <Button type="submit">查询</Button>
        <Button type="button" variant="secondary" onClick={() => { setKeyword(""); setQuery(""); }}>
          重置
        </Button>
      </form>

      <Show when={crontabs()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {crontabs()?.message || "定时任务加载失败"}
        </div>
      </Show>

      <section class="overflow-hidden rounded-lg border border-slate-200 bg-white shadow-sm">
        <div class="overflow-x-auto">
          <table class="min-w-full divide-y divide-slate-200 text-sm">
            <thead class="bg-slate-50 text-left text-xs font-semibold uppercase tracking-wide text-slate-500">
              <tr>
                <th class="px-4 py-3">任务名</th>
                <th class="px-4 py-3">Cron</th>
                <th class="px-4 py-3">MFA</th>
                <th class="px-4 py-3">状态</th>
                <th class="px-4 py-3">开始时间</th>
                <th class="px-4 py-3">结束时间</th>
              </tr>
            </thead>
            <tbody class="divide-y divide-slate-100">
              <Show
                when={(crontabs()?.data.items.length ?? 0) > 0}
                fallback={
                  <tr>
                    <td class="px-4 py-8 text-center text-slate-400" colSpan={6}>
                      {crontabs.loading ? "加载中..." : "暂无定时任务"}
                    </td>
                  </tr>
                }
              >
                <For each={crontabs()?.data.items ?? []}>
                  {(item) => (
                    <tr class="hover:bg-slate-50">
                      <td class="px-4 py-3 font-medium text-slate-950">{item.cronName}</td>
                      <td class="px-4 py-3 font-mono text-xs text-slate-600">{item.cronExp}</td>
                      <td class="px-4 py-3 text-slate-500">{item.cronMfa}</td>
                      <td class="px-4 py-3">
                        <span
                          class={
                            item.cronStatus === 0
                              ? "rounded-full bg-emerald-50 px-2 py-1 text-xs font-medium text-emerald-700"
                              : "rounded-full bg-slate-100 px-2 py-1 text-xs font-medium text-slate-500"
                          }
                        >
                          {item.cronStatus === 0 ? "启用" : "停用"}
                        </span>
                      </td>
                      <td class="px-4 py-3 text-slate-500">{item.startTime}</td>
                      <td class="px-4 py-3 text-slate-500">{item.endTime || "-"}</td>
                    </tr>
                  )}
                </For>
              </Show>
            </tbody>
          </table>
        </div>
      </section>
    </div>
  );
}
