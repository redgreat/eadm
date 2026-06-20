import { apiRequest } from "./client";
import type { ApiResponse } from "./client";

export type SystemInfoItem = {
  key: string;
  value: string | number | boolean;
};

export type SystemInfo = {
  items: SystemInfoItem[];
};

export function getSystemInfo(): Promise<ApiResponse<SystemInfo>> {
  return apiRequest<SystemInfo>("/api/system/info");
}
