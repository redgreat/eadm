import { createMemo, createResource, createSignal, For, Show } from "solid-js";
import Button from "../components/ui/Button";
import DataTable, { type DataTableColumn } from "../components/ui/DataTable";
import { getLocationPoints, type LocationPoint } from "../lib/api/location";
import { hoursAgo, toApiDateTime, toInputDateTime } from "../lib/datetime";

const columns: DataTableColumn<LocationPoint>[] = [
  { key: "utcTime", header: "时间", sortable: true },
  { key: "deviceNo", header: "设备号", sortable: true, class: "font-medium text-slate-950" },
  { key: "lng", header: "经度", sortable: true },
  { key: "lat", header: "纬度", sortable: true }
];

export default function LocationPage() {
  const [deviceNo, setDeviceNo] = createSignal("");
  const [startTime, setStartTime] = createSignal(toInputDateTime(hoursAgo(2)));
  const [endTime, setEndTime] = createSignal(toInputDateTime(new Date()));
  const [query, setQuery] = createSignal(currentQuery());
  const [points] = createResource(query, getLocationPoints);
  const rows = createMemo(() => points()?.data.items ?? []);

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
          <p class="mt-1 text-sm text-slate-500">按设备和时间范围查看轨迹点，并预览路径走势。</p>
        </div>
        <div class="rounded-md bg-white px-3 py-2 text-sm text-slate-500 shadow-sm">共 {rows().length} 个坐标点</div>
      </section>

      <form class="grid gap-3 rounded-lg border border-slate-200 bg-white p-4 shadow-sm lg:grid-cols-[180px_1fr_1fr_auto]" onSubmit={handleSearch}>
        <input value={deviceNo()} onInput={(event) => setDeviceNo(event.currentTarget.value)} placeholder="设备号，可为空" class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950" />
        <input type="datetime-local" value={startTime()} onInput={(event) => setStartTime(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950" />
        <input type="datetime-local" value={endTime()} onInput={(event) => setEndTime(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950" />
        <Button type="submit">查询</Button>
      </form>

      <RoutePreview points={rows()} />

      <DataTable
        rows={rows()}
        columns={columns}
        loading={points.loading}
        error={points()?.success === false ? points()?.message || "轨迹数据加载失败" : undefined}
        emptyText="暂无轨迹数据"
        rowKey={(row, index) => `${row.deviceNo}-${row.utcTime}-${index}`}
      />
    </div>
  );
}

function RoutePreview(props: { points: LocationPoint[] }) {
  const path = createMemo(() => buildPolyline(props.points));

  return (
    <section class="rounded-lg border border-slate-200 bg-white p-4 shadow-sm">
      <div class="mb-3 flex items-center justify-between">
        <h3 class="text-base font-semibold">路径预览</h3>
        <span class="text-xs text-slate-500">基于查询结果缩放到当前视图</span>
      </div>
      <div class="relative h-64 overflow-hidden rounded-md bg-slate-950">
        <Show when={props.points.length > 1} fallback={<div class="grid h-full place-items-center text-sm text-slate-400">暂无足够坐标绘制路径</div>}>
          <svg viewBox="0 0 100 100" preserveAspectRatio="none" class="h-full w-full">
            <polyline points={path()} fill="none" stroke="#38bdf8" stroke-width="1.5" vector-effect="non-scaling-stroke" />
            <For each={path().split(" ").filter(Boolean)}>
              {(point, index) => {
                const [x, y] = point.split(",");
                return <circle cx={x} cy={y} r={index() === 0 ? 1.5 : 1} fill={index() === 0 ? "#34d399" : "#f8fafc"} />;
              }}
            </For>
          </svg>
        </Show>
      </div>
    </section>
  );
}

function buildPolyline(points: LocationPoint[]) {
  const coords = points
    .map((point) => ({ lng: Number(point.lng), lat: Number(point.lat) }))
    .filter((point) => Number.isFinite(point.lng) && Number.isFinite(point.lat));
  if (coords.length === 0) {
    return "";
  }
  const minLng = Math.min(...coords.map((point) => point.lng));
  const maxLng = Math.max(...coords.map((point) => point.lng));
  const minLat = Math.min(...coords.map((point) => point.lat));
  const maxLat = Math.max(...coords.map((point) => point.lat));
  const lngSpan = maxLng - minLng || 1;
  const latSpan = maxLat - minLat || 1;
  return coords
    .map((point) => {
      const x = ((point.lng - minLng) / lngSpan) * 90 + 5;
      const y = 95 - ((point.lat - minLat) / latSpan) * 90;
      return `${x.toFixed(2)},${y.toFixed(2)}`;
    })
    .join(" ");
}
