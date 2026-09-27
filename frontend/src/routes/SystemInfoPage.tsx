import { createResource } from "solid-js";
import { getSystemInfo } from "../lib/api/system";
import DataTable, { type DataTableColumn } from "../components/ui/DataTable";
import Badge from "../components/ui/Badge";
import Button from "../components/ui/Button";
import type { SystemInfoItem } from "../lib/api/system";

const labels: Record<string, string> = {
  otpRelease: "OTP 版本",
  version: "Erlang 版本",
  systemArchitecture: "系统架构",
  schedulers: "调度器",
  schedulersOnline: "在线调度器",
  runQueue: "运行队列",
  processCount: "进程数",
  processLimit: "进程上限",
  portCount: "端口数",
  portLimit: "端口上限",
  etsCount: "ETS 表数",
  etsLimit: "ETS 上限",
  memoryTotal: "总内存",
  memoryProcessesUsed: "进程内存",
  memoryBinary: "Binary 内存",
  memoryCode: "Code 内存",
  memoryEts: "ETS 内存",
  ioInput: "IO 输入",
  ioOutput: "IO 输出",
  uptimeSeconds: "运行时长(秒)"
};

export default function SystemInfoPage() {
  const [info, { refetch }] = createResource(getSystemInfo);
  const columns: DataTableColumn<SystemInfoItem>[] = [
    {
      key: "label",
      header: "指标",
      accessor: (item) => labels[item.key] ?? item.key,
      render: (item) => (
        <div>
          <div class="font-medium text-slate-950">{labels[item.key] ?? item.key}</div>
          <div class="mt-1 text-xs text-slate-400">{item.key}</div>
        </div>
      ),
      sortable: true
    },
    {
      key: "value",
      header: "当前值",
      accessor: (item) => item.value,
      render: (item) => <span class="break-all font-mono text-slate-700">{String(item.value)}</span>,
      sortable: true
    },
    {
      key: "group",
      header: "分组",
      accessor: (item) => groupOf(item.key),
      render: (item) => <Badge tone="blue">{groupOf(item.key)}</Badge>,
      sortable: true
    }
  ];

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">系统信息</h2>
          <p class="mt-1 text-sm text-slate-500">查看 Erlang VM、进程、端口、ETS 和内存运行状态。</p>
        </div>
        <Button variant="secondary" type="button" onClick={() => refetch()}>
          刷新
        </Button>
      </section>

      <DataTable
        columns={columns}
        rows={info()?.data.items ?? []}
        loading={info.loading}
        error={info()?.success === false ? info()?.message || "系统信息加载失败" : undefined}
        emptyText="暂无系统运行数据"
        pageSize={12}
        rowKey={(item) => item.key}
      />
    </div>
  );
}

function groupOf(key: string) {
  if (key.startsWith("memory")) {
    return "内存";
  }
  if (key.startsWith("io")) {
    return "IO";
  }
  if (key.includes("process") || key.includes("port") || key.includes("ets")) {
    return "资源";
  }
  return "运行时";
}
