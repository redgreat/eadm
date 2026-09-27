import { createResource, createSignal } from "solid-js";
import Badge from "../components/ui/Badge";
import Button from "../components/ui/Button";
import DataTable, { type DataTableColumn } from "../components/ui/DataTable";
import { getDevices, type DeviceItem } from "../lib/api/devices";

const columns: DataTableColumn<DeviceItem>[] = [
  { key: "deviceNo", header: "设备号", sortable: true, class: "font-medium text-slate-950" },
  { key: "imei", header: "IMEI", sortable: true },
  { key: "simNo", header: "SIM 卡号", sortable: true },
  { key: "remark", header: "备注" },
  {
    key: "enable",
    header: "状态",
    sortable: true,
    render: (device) => {
      const enabled = device.enable === true || device.enable === 1;
      return <Badge tone={enabled ? "green" : "red"}>{enabled ? "启用" : "禁用"}</Badge>;
    }
  },
  { key: "createdAt", header: "创建时间", sortable: true }
];

export default function DevicesPage() {
  const [keyword, setKeyword] = createSignal("");
  const [query, setQuery] = createSignal("");
  const [devices] = createResource(query, getDevices);

  const handleSearch = (event: SubmitEvent) => {
    event.preventDefault();
    setQuery(keyword().trim());
  };

  return (
    <div class="space-y-6">
      <section>
        <h2 class="text-2xl font-semibold tracking-tight">设备管理</h2>
        <p class="mt-1 text-sm text-slate-500">按设备号查询设备基础信息和启用状态。</p>
      </section>

      <form class="flex flex-wrap gap-2 rounded-lg border border-slate-200 bg-white p-4 shadow-sm" onSubmit={handleSearch}>
        <input
          value={keyword()}
          onInput={(event) => setKeyword(event.currentTarget.value)}
          placeholder="设备号"
          class="h-10 min-w-64 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950"
        />
        <Button type="submit">查询</Button>
        <Button type="button" variant="secondary" onClick={() => { setKeyword(""); setQuery(""); }}>重置</Button>
      </form>

      <DataTable
        rows={devices()?.data.items ?? []}
        columns={columns}
        loading={devices.loading}
        error={devices()?.success === false ? devices()?.message || "设备列表加载失败" : undefined}
        emptyText="暂无设备数据"
        rowKey={(row) => row.deviceNo}
      />
    </div>
  );
}
