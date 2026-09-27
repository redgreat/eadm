import { createResource, createSignal } from "solid-js";
import Badge from "../components/ui/Badge";
import Button from "../components/ui/Button";
import DataTable, { type DataTableColumn } from "../components/ui/DataTable";
import { getCrontabs, type CrontabItem } from "../lib/api/crontabs";

const columns: DataTableColumn<CrontabItem>[] = [
  { key: "cronName", header: "任务名", sortable: true, class: "font-medium text-slate-950" },
  { key: "cronExp", header: "Cron 表达式", sortable: true, class: "font-mono text-xs" },
  { key: "cronMfa", header: "MFA", sortable: true },
  {
    key: "cronStatus",
    header: "状态",
    sortable: true,
    render: (item) => <Badge tone={item.cronStatus === 0 ? "green" : "red"}>{item.cronStatus === 0 ? "启用" : "停用"}</Badge>
  },
  { key: "startTime", header: "开始时间", sortable: true },
  { key: "endTime", header: "结束时间", sortable: true, render: (item) => <>{item.endTime || "-"}</> }
];

export default function CrontabsPage() {
  const [keyword, setKeyword] = createSignal("");
  const [query, setQuery] = createSignal("");
  const [crontabs] = createResource(query, getCrontabs);

  const handleSearch = (event: SubmitEvent) => {
    event.preventDefault();
    setQuery(keyword().trim());
  };

  return (
    <div class="space-y-6">
      <section>
        <h2 class="text-2xl font-semibold tracking-tight">定时任务</h2>
        <p class="mt-1 text-sm text-slate-500">查询任务计划、执行模块和启用状态。</p>
      </section>

      <form class="flex flex-wrap gap-2 rounded-lg border border-slate-200 bg-white p-4 shadow-sm" onSubmit={handleSearch}>
        <input
          value={keyword()}
          onInput={(event) => setKeyword(event.currentTarget.value)}
          placeholder="任务名"
          class="h-10 min-w-64 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950"
        />
        <Button type="submit">查询</Button>
        <Button type="button" variant="secondary" onClick={() => { setKeyword(""); setQuery(""); }}>重置</Button>
      </form>

      <DataTable
        rows={crontabs()?.data.items ?? []}
        columns={columns}
        loading={crontabs.loading}
        error={crontabs()?.success === false ? crontabs()?.message || "定时任务加载失败" : undefined}
        emptyText="暂无定时任务"
        rowKey={(row) => row.id}
      />
    </div>
  );
}
