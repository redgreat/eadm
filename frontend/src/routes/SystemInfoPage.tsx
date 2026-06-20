import { createResource, For, Show } from "solid-js";
import { getSystemInfo } from "../lib/api/system";

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
  const [info] = createResource(getSystemInfo);

  return (
    <div class="space-y-6">
      <section>
        <h2 class="text-2xl font-semibold tracking-tight">系统信息</h2>
        <p class="mt-1 text-sm text-slate-500">已接入新的 `/api/system/info` 接口。</p>
      </section>

      <Show when={info()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {info()?.message || "系统信息加载失败"}
        </div>
      </Show>

      <section class="grid gap-4 md:grid-cols-2 xl:grid-cols-3">
        <Show when={(info()?.data.items.length ?? 0) > 0} fallback={<div class="text-sm text-slate-400">加载中...</div>}>
          <For each={info()?.data.items ?? []}>
            {(item) => (
              <article class="rounded-lg border border-slate-200 bg-white p-4 shadow-sm">
                <div class="text-xs font-medium uppercase tracking-wide text-slate-400">{item.key}</div>
                <div class="mt-2 text-sm text-slate-500">{labels[item.key] ?? item.key}</div>
                <div class="mt-2 break-all text-lg font-semibold text-slate-950">{String(item.value)}</div>
              </article>
            )}
          </For>
        </Show>
      </section>
    </div>
  );
}
