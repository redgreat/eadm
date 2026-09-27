import { createMemo, createResource, createSignal, For } from "solid-js";
import Button from "../components/ui/Button";
import DataTable, { type DataTableColumn } from "../components/ui/DataTable";
import { getHealthRecords, type HealthRecord } from "../lib/api/health";
import { hoursAgo, toApiDateTime, toInputDateTime } from "../lib/datetime";

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

  function currentQuery() {
    return {
      dataType: dataType(),
      startTime: toApiDateTime(startTime()),
      endTime: toApiDateTime(endTime())
    };
  }

  const columns = createMemo<DataTableColumn<HealthRecord>[]>(() => {
    const keys = Object.keys(records()?.data.items[0] ?? { utcTime: "" });
    return keys.map((key) => ({
      key,
      header: columnLabels[key] ?? key,
      sortable: true,
      render: (row) => <>{String(row[key] ?? "")}</>
    }));
  });

  const handleSearch = (event: SubmitEvent) => {
    event.preventDefault();
    setQuery(currentQuery());
  };

  return (
    <div class="space-y-6">
      <section>
        <h2 class="text-2xl font-semibold tracking-tight">健康数据</h2>
        <p class="mt-1 text-sm text-slate-500">按数据类型和时间范围查询手表健康记录。</p>
      </section>

      <form class="grid gap-3 rounded-lg border border-slate-200 bg-white p-4 shadow-sm lg:grid-cols-[160px_1fr_1fr_auto]" onSubmit={handleSearch}>
        <select value={dataType()} onChange={(event) => setDataType(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950">
          <For each={healthTypes}>{(item) => <option value={item.value}>{item.label}</option>}</For>
        </select>
        <input type="datetime-local" value={startTime()} onInput={(event) => setStartTime(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950" />
        <input type="datetime-local" value={endTime()} onInput={(event) => setEndTime(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950" />
        <Button type="submit">查询</Button>
      </form>

      <DataTable
        rows={records()?.data.items ?? []}
        columns={columns()}
        loading={records.loading}
        error={records()?.success === false ? records()?.message || "健康数据加载失败" : undefined}
        emptyText="暂无健康数据"
        rowKey={(row, index) => String(row.utcTime ?? index)}
      />
    </div>
  );
}
