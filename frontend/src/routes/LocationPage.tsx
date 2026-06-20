import { createResource, createSignal, For, Show } from "solid-js";
import Button from "../components/ui/Button";
import { getLocationPoints } from "../lib/api/location";

export default function LocationPage() {
  const [deviceNo, setDeviceNo] = createSignal("");
  const [startTime, setStartTime] = createSignal(toInputDateTime(hoursAgo(2)));
  const [endTime, setEndTime] = createSignal(toInputDateTime(new Date()));
  const [query, setQuery] = createSignal(currentQuery());
  const [points] = createResource(query, getLocationPoints);

  function currentQuery() {
    return {
      deviceNo: deviceNo(),
      startTime: toApiDateTime(startTime()),
      endTime: toApiDateTime(endTime())
    };
  }

  const handleSearch = (event: SubmitEvent) => {
    event.preventDefault();
    setQuery(currentQuery());
  };

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">轨迹位置</h2>
          <p class="mt-1 text-sm text-slate-500">已接入新的 `/api/location` 查询接口，地图展示后续补上。</p>
        </div>
        <div class="rounded-md bg-white px-3 py-2 text-sm text-slate-500 shadow-sm">
          共 {points()?.data.total ?? "--"} 个坐标点
        </div>
      </section>

      <form class="grid gap-3 rounded-lg border border-slate-200 bg-white p-4 shadow-sm lg:grid-cols-[180px_1fr_1fr_auto]" onSubmit={handleSearch}>
        <input
          value={deviceNo()}
          onInput={(event) => setDeviceNo(event.currentTarget.value)}
          placeholder="设备号，可为空"
          class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950"
        />
        <input
          type="datetime-local"
          value={startTime()}
          onInput={(event) => setStartTime(event.currentTarget.value)}
          class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950"
        />
        <input
          type="datetime-local"
          value={endTime()}
          onInput={(event) => setEndTime(event.currentTarget.value)}
          class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950"
        />
        <Button type="submit">查询</Button>
      </form>

      <Show when={points()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {points()?.message || "轨迹数据加载失败"}
        </div>
      </Show>

      <section class="overflow-hidden rounded-lg border border-slate-200 bg-white shadow-sm">
        <div class="overflow-x-auto">
          <table class="min-w-full divide-y divide-slate-200 text-sm">
            <thead class="bg-slate-50 text-left text-xs font-semibold uppercase tracking-wide text-slate-500">
              <tr>
                <th class="px-4 py-3">时间</th>
                <th class="px-4 py-3">设备号</th>
                <th class="px-4 py-3">经度</th>
                <th class="px-4 py-3">纬度</th>
              </tr>
            </thead>
            <tbody class="divide-y divide-slate-100">
              <Show
                when={(points()?.data.items.length ?? 0) > 0}
                fallback={
                  <tr>
                    <td class="px-4 py-8 text-center text-slate-400" colSpan={4}>
                      {points.loading ? "加载中..." : "暂无轨迹数据"}
                    </td>
                  </tr>
                }
              >
                <For each={points()?.data.items ?? []}>
                  {(point) => (
                    <tr class="hover:bg-slate-50">
                      <td class="px-4 py-3 text-slate-500">{point.utcTime}</td>
                      <td class="px-4 py-3 font-medium text-slate-950">{point.deviceNo}</td>
                      <td class="px-4 py-3">{point.lng}</td>
                      <td class="px-4 py-3">{point.lat}</td>
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

function hoursAgo(hours: number) {
  return new Date(Date.now() - hours * 60 * 60 * 1000);
}

function toInputDateTime(date: Date) {
  const pad = (value: number) => String(value).padStart(2, "0");
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}T${pad(date.getHours())}:${pad(date.getMinutes())}`;
}

function toApiDateTime(value: string) {
  return `${value.replace("T", " ")}:00`;
}
