import { createResource, For, Show } from "solid-js";
import { getUsers } from "../lib/api/users";

export default function UsersPage() {
  const [users] = createResource(getUsers);

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">用户管理</h2>
          <p class="mt-1 text-sm text-slate-500">已接入新的 `/api/users` 只读列表接口。</p>
        </div>
        <div class="rounded-md bg-white px-3 py-2 text-sm text-slate-500 shadow-sm">
          共 {users()?.data.total ?? "--"} 个用户
        </div>
      </section>

      <Show when={users()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {users()?.message || "用户列表加载失败"}
        </div>
      </Show>

      <section class="overflow-hidden rounded-lg border border-slate-200 bg-white shadow-sm">
        <div class="overflow-x-auto">
          <table class="min-w-full divide-y divide-slate-200 text-sm">
            <thead class="bg-slate-50 text-left text-xs font-semibold uppercase tracking-wide text-slate-500">
              <tr>
                <th class="px-4 py-3">登录名</th>
                <th class="px-4 py-3">用户名</th>
                <th class="px-4 py-3">邮箱</th>
                <th class="px-4 py-3">租户</th>
                <th class="px-4 py-3">状态</th>
                <th class="px-4 py-3">创建时间</th>
              </tr>
            </thead>
            <tbody class="divide-y divide-slate-100">
              <Show
                when={(users()?.data.items.length ?? 0) > 0}
                fallback={
                  <tr>
                    <td class="px-4 py-8 text-center text-slate-400" colSpan={6}>
                      {users.loading ? "加载中..." : "暂无用户数据"}
                    </td>
                  </tr>
                }
              >
                <For each={users()?.data.items ?? []}>
                  {(user) => (
                    <tr class="hover:bg-slate-50">
                      <td class="px-4 py-3 font-medium text-slate-950">{user.loginName}</td>
                      <td class="px-4 py-3">{user.userName}</td>
                      <td class="px-4 py-3 text-slate-500">{user.email}</td>
                      <td class="px-4 py-3 text-slate-500">{user.tenantName}</td>
                      <td class="px-4 py-3">
                        <span
                          class={
                            user.userStatus === 0
                              ? "rounded-full bg-emerald-50 px-2 py-1 text-xs font-medium text-emerald-700"
                              : "rounded-full bg-slate-100 px-2 py-1 text-xs font-medium text-slate-500"
                          }
                        >
                          {user.userStatus === 0 ? "启用" : "禁用"}
                        </span>
                      </td>
                      <td class="px-4 py-3 text-slate-500">{user.createdAt}</td>
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
