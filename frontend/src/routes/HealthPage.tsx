import { createMemo, createResource, createSignal, For, Show } from "solid-js";
import Button from "../components/ui/Button";
import { getHealthRecords } from "../lib/api/health";

const healthTypes = [
  { value: "1", label: "步数" },
  { value: "2", label: "心率" },
  { value: "3", label: "体温" },
  { value: "4", label: "血压" },
  { value: "5", label: "睡眠" },
  { value: "6", label: "信号/电量" }
];

const columnLabels: Record<string, string> = {
  utcTime: "时间",
  steps: "步数",
  heartbeat: "心率",
  bodyTemperature: "体温",
  wristTemperature: "腕温",
  diastolic: "舒张压",
  shrink: "收缩压",
  sleepType: "睡眠类型",
  startTime: "开始时间",
  endTime: "结束时间",
  minute: "分钟",
  battery: "电量",
  signal: "信号"
};

export default function HealthPage() {
  const [dataType, setDataType] = createSignal("1");
  const [startTime, setStartTime] = createSignal(toInputDateTime(hoursAgo(24)));
  const [endTime, setEndTime] = createSignal(toInputDateTime(new Date()));
  const [query, setQuery] = createSignal(currentQuery());
  const [records] = createResource(query, getHealthRecords);
  const columns = createMemo(() => Object.keys(records()?.data.items[0] ?? {}));

  function currentQuery() {
    return {
      dataType: dataType(),
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
      <section>
        <h2 class="text-2xl font-semibold tracking-tight">健康数据</h2>
        <p class="mt-1 text-sm text-slate-500">已接入新的 `/api/health` 查询接口。</p>
      </section>

      <form class="grid gap-3 rounded-lg border border-slate-200 bg-white p-4 shadow-sm lg:grid-cols-[160px_1fr_1fr_auto]" onSubmit={handleSearch}>
        <select
          value={dataType()}
          onChange={(event) => setDataType(event.currentTarget.value)}
          class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950"
        >
          <For each={healthTypes}>
            {(item) => <option value={item.value}>{item.label}</option>}
          </For>
        </select>
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

      <Show when={records()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {records()?.message || "健康数据加载失败"}
        </div>
      </Show>

      <section class="overflow-hidden rounded-lg border border-slate-200 bg-white shadow-sm">
        <div class="overflow-x-auto">
          <table class="min-w-full divide-y divide-slate-200 text-sm">
            <thead class="bg-slate-50 text-left text-xs font-semibold uppercase tracking-wide text-slate-500">
              <tr>
                <For each={columns()}>
                  {(column) => <th class="px-4 py-3">{columnLabels[column] ?? column}</th>}
                </For>
              </tr>
            </thead>
            <tbody class="divide-y divide-slate-100">
              <Show
                when={(records()?.data.items.length ?? 0) > 0}
                fallback={
                  <tr>
                    <td class="px-4 py-8 text-center text-slate-400" colSpan={Math.max(columns().length, 1)}>
                      {records.loading ? "加载中..." : "暂无健康数据"}
                    </td>
                  </tr>
                }
              >
                <For each={records()?.data.items ?? []}>
                  {(record) => (
                    <tr class="hover:bg-slate-50">
                      <For each={columns()}>
                        {(column) => <td class="px-4 py-3 text-slate-600">{String(record[column] ?? "")}</td>}
                      </For>
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
