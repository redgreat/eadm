import { A, useNavigate } from "@solidjs/router";
import { Activity, CalendarClock, Gauge, HeartPulse, MapPinned, ServerCog, Shield, UsersRound } from "lucide-solid";
import type { ParentProps } from "solid-js";
import { createEffect, createResource, For, Show } from "solid-js";
import Button from "../ui/Button";
import { getCurrentUser, logout } from "../../lib/api/auth";

const navItems = [
  { href: "/", label: "仪表盘", icon: Gauge },
  { href: "/health", label: "健康数据", icon: HeartPulse },
  { href: "/location", label: "轨迹位置", icon: MapPinned },
  { href: "/finance", label: "财务数据", icon: Activity },
  { href: "/crontab", label: "定时任务", icon: CalendarClock },
  { href: "/user", label: "用户管理", icon: UsersRound },
  { href: "/role", label: "角色权限", icon: Shield },
  { href: "/sysinfo", label: "系统信息", icon: ServerCog }
];

export default function AdminLayout(props: ParentProps) {
  const navigate = useNavigate();
  const [session] = createResource(getCurrentUser);

  createEffect(() => {
    const result = session();
    if (result && !result.success && result.code === "unauthorized") {
      navigate("/login", { replace: true });
    }
  });

  const handleLogout = async () => {
    await logout();
    window.location.href = "/login";
  };

  return (
    <div class="min-h-screen bg-slate-100 text-slate-950">
      <aside class="fixed inset-y-0 left-0 hidden w-64 border-r border-slate-200 bg-white lg:block">
        <div class="flex h-16 items-center border-b border-slate-200 px-6">
          <div>
            <div class="text-lg font-semibold tracking-tight">EADM</div>
            <div class="text-xs text-slate-500">管理端后台</div>
          </div>
        </div>
        <nav class="space-y-1 p-3">
          <For each={navItems}>
            {(item) => {
              const Icon = item.icon;
              return (
                <A
                  href={item.href}
                  class="flex items-center gap-3 rounded-md px-3 py-2 text-sm font-medium text-slate-600 hover:bg-slate-100 hover:text-slate-950"
                  activeClass="bg-slate-950 text-white hover:bg-slate-950 hover:text-white"
                  end={item.href === "/"}
                >
                  <Icon size={18} />
                  {item.label}
                </A>
              );
            }}
          </For>
        </nav>
      </aside>

      <div class="lg:pl-64">
        <header class="sticky top-0 z-10 flex h-16 items-center justify-between border-b border-slate-200 bg-white/90 px-4 backdrop-blur lg:px-8">
          <div>
            <h1 class="text-base font-semibold">后台工作台</h1>
            <p class="text-xs text-slate-500">SolidJS 迁移版</p>
          </div>
          <div class="flex items-center gap-3 text-right text-sm">
            <Show when={session()?.success} fallback={<span class="text-slate-500">未登录</span>}>
              <div>
                <div class="font-medium">{session()?.data.userName || session()?.data.loginName}</div>
                <div class="text-xs text-slate-500">{session()?.data.loginName}</div>
              </div>
              <Button variant="secondary" type="button" onClick={handleLogout}>
                退出
              </Button>
            </Show>
          </div>
        </header>
        <main class="p-4 lg:p-8">{props.children}</main>
      </div>
    </div>
  );
}
