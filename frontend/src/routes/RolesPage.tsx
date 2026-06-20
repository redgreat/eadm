import { createResource, For, Show } from "solid-js";
import { getRoles } from "../lib/api/roles";

export default function RolesPage() {
  const [roles] = createResource(getRoles);

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">角色权限</h2>
          <p class="mt-1 text-sm text-slate-500">已接入新的 `/api/roles` 只读列表接口。</p>
        </div>
        <div class="rounded-md bg-white px-3 py-2 text-sm text-slate-500 shadow-sm">
          共 {roles()?.data.total ?? "--"} 个角色
        </div>
      </section>

      <Show when={roles()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {roles()?.message || "角色列表加载失败"}
        </div>
      </Show>

      <section class="overflow-hidden rounded-lg border border-slate-200 bg-white shadow-sm">
        <div class="overflow-x-auto">
          <table class="min-w-full divide-y divide-slate-200 text-sm">
            <thead class="bg-slate-50 text-left text-xs font-semibold uppercase tracking-wide text-slate-500">
              <tr>
                <th class="px-4 py-3">角色名称</th>
                <th class="px-4 py-3">状态</th>
                <th class="px-4 py-3">创建时间</th>
              </tr>
            </thead>
            <tbody class="divide-y divide-slate-100">
              <Show
                when={(roles()?.data.items.length ?? 0) > 0}
                fallback={
                  <tr>
                    <td class="px-4 py-8 text-center text-slate-400" colSpan={3}>
                      {roles.loading ? "加载中..." : "暂无角色数据"}
                    </td>
                  </tr>
                }
              >
                <For each={roles()?.data.items ?? []}>
                  {(role) => (
                    <tr class="hover:bg-slate-50">
                      <td class="px-4 py-3 font-medium text-slate-950">{role.roleName}</td>
                      <td class="px-4 py-3">
                        <span
                          class={
                            role.roleStatus === 0
                              ? "rounded-full bg-emerald-50 px-2 py-1 text-xs font-medium text-emerald-700"
                              : "rounded-full bg-slate-100 px-2 py-1 text-xs font-medium text-slate-500"
                          }
                        >
                          {role.roleStatus === 0 ? "启用" : "禁用"}
                        </span>
                      </td>
                      <td class="px-4 py-3 text-slate-500">{role.createdAt}</td>
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
