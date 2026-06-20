import { createResource, createSignal, For, Show } from "solid-js";
import Button from "../components/ui/Button";
import { getDevices } from "../lib/api/devices";

export default function DevicesPage() {
  const [keyword, setKeyword] = createSignal("");
  const [query, setQuery] = createSignal("");
  const [devices] = createResource(query, getDevices);

  const handleSearch = (event: SubmitEvent) => {
    event.preventDefault();
    setQuery(keyword());
  };

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">设备管理</h2>
          <p class="mt-1 text-sm text-slate-500">已接入新的 `/api/devices` 只读列表接口。</p>
        </div>
        <div class="rounded-md bg-white px-3 py-2 text-sm text-slate-500 shadow-sm">
          共 {devices()?.data.total ?? "--"} 台设备
        </div>
      </section>

      <form class="flex flex-wrap gap-2 rounded-lg border border-slate-200 bg-white p-4 shadow-sm" onSubmit={handleSearch}>
        <input
          value={keyword()}
          onInput={(event) => setKeyword(event.currentTarget.value)}
          placeholder="按设备号搜索"
          class="h-10 min-w-64 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950"
        />
        <Button type="submit">查询</Button>
        <Button type="button" variant="secondary" onClick={() => { setKeyword(""); setQuery(""); }}>
          重置
        </Button>
      </form>

      <Show when={devices()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {devices()?.message || "设备列表加载失败"}
        </div>
      </Show>

      <section class="overflow-hidden rounded-lg border border-slate-200 bg-white shadow-sm">
        <div class="overflow-x-auto">
          <table class="min-w-full divide-y divide-slate-200 text-sm">
            <thead class="bg-slate-50 text-left text-xs font-semibold uppercase tracking-wide text-slate-500">
              <tr>
                <th class="px-4 py-3">设备号</th>
                <th class="px-4 py-3">IMEI</th>
                <th class="px-4 py-3">SIM</th>
                <th class="px-4 py-3">备注</th>
                <th class="px-4 py-3">状态</th>
                <th class="px-4 py-3">创建时间</th>
              </tr>
            </thead>
            <tbody class="divide-y divide-slate-100">
              <Show
                when={(devices()?.data.items.length ?? 0) > 0}
                fallback={
                  <tr>
                    <td class="px-4 py-8 text-center text-slate-400" colSpan={6}>
                      {devices.loading ? "加载中..." : "暂无设备数据"}
                    </td>
                  </tr>
                }
              >
                <For each={devices()?.data.items ?? []}>
                  {(device) => (
                    <tr class="hover:bg-slate-50">
                      <td class="px-4 py-3 font-medium text-slate-950">{device.deviceNo}</td>
                      <td class="px-4 py-3 text-slate-500">{device.imei}</td>
                      <td class="px-4 py-3 text-slate-500">{device.simNo}</td>
                      <td class="px-4 py-3">{device.remark}</td>
                      <td class="px-4 py-3">
                        <span
                          class={
                            device.enable === true || device.enable === 1
                              ? "rounded-full bg-emerald-50 px-2 py-1 text-xs font-medium text-emerald-700"
                              : "rounded-full bg-slate-100 px-2 py-1 text-xs font-medium text-slate-500"
                          }
                        >
                          {device.enable === true || device.enable === 1 ? "启用" : "禁用"}
                        </span>
                      </td>
                      <td class="px-4 py-3 text-slate-500">{device.createdAt}</td>
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
