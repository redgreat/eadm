import { createResource } from "solid-js";
import Badge from "../components/ui/Badge";
import DataTable, { type DataTableColumn } from "../components/ui/DataTable";
import { getUsers, type UserItem } from "../lib/api/users";

const columns: DataTableColumn<UserItem>[] = [
  { key: "loginName", header: "登录名", sortable: true, class: "font-medium text-slate-950" },
  { key: "userName", header: "显示姓名", sortable: true },
  { key: "email", header: "邮箱", sortable: true },
  { key: "tenantName", header: "租户", sortable: true },
  {
    key: "userStatus",
    header: "状态",
    sortable: true,
    render: (user) => <Badge tone={user.userStatus === 0 ? "green" : "red"}>{user.userStatus === 0 ? "启用" : "禁用"}</Badge>
  },
  { key: "createdAt", header: "创建时间", sortable: true }
];

export default function UsersPage() {
  const [users, { refetch }] = createResource(getUsers);

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">用户管理</h2>
          <p class="mt-1 text-sm text-slate-500">查看系统用户、租户归属和账号状态。</p>
        </div>
        <button type="button" class="rounded-md border border-slate-200 bg-white px-3 py-2 text-sm text-slate-600 shadow-sm hover:bg-slate-50" onClick={() => refetch()}>
          刷新
        </button>
      </section>

      <DataTable
        rows={users()?.data.items ?? []}
        columns={columns}
        loading={users.loading}
        error={users()?.success === false ? users()?.message || "用户列表加载失败" : undefined}
        emptyText="暂无用户数据"
        rowKey={(row) => row.id}
      />
    </div>
  );
}
