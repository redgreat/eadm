import { createResource } from "solid-js";
import Badge from "../components/ui/Badge";
import DataTable, { type DataTableColumn } from "../components/ui/DataTable";
import { getRoles, type RoleItem } from "../lib/api/roles";

const columns: DataTableColumn<RoleItem>[] = [
  { key: "roleName", header: "角色名称", sortable: true, class: "font-medium text-slate-950" },
  {
    key: "roleStatus",
    header: "状态",
    sortable: true,
    render: (role) => <Badge tone={role.roleStatus === 0 ? "green" : "red"}>{role.roleStatus === 0 ? "启用" : "禁用"}</Badge>
  },
  { key: "createdAt", header: "创建时间", sortable: true }
];

export default function RolesPage() {
  const [roles, { refetch }] = createResource(getRoles);

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">角色权限</h2>
          <p class="mt-1 text-sm text-slate-500">查看系统角色及启用状态。</p>
        </div>
        <button type="button" class="rounded-md border border-slate-200 bg-white px-3 py-2 text-sm text-slate-600 shadow-sm hover:bg-slate-50" onClick={() => refetch()}>
          刷新
        </button>
      </section>

      <DataTable
        rows={roles()?.data.items ?? []}
        columns={columns}
        loading={roles.loading}
        error={roles()?.success === false ? roles()?.message || "角色列表加载失败" : undefined}
        emptyText="暂无角色数据"
        rowKey={(row) => row.id}
      />
    </div>
  );
}
