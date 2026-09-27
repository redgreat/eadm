import { apiRequest, apiV1 } from "./client";
import type { ApiResponse } from "./client";

export type CrontabItem = {
  id: number | string;
  cronName: string;
  cronExp: string;
  cronMfa: string;
  startTime: string;
  endTime: string | null;
  cronStatus: number;
  createdAt: string;
};

export type CrontabList = {
  items: CrontabItem[];
  total: number;
};

export function getCrontabs(cronName = ""): Promise<ApiResponse<CrontabList>> {
  const query = new URLSearchParams();
  if (cronName.trim()) {
    query.set("cronName", cronName.trim());
  }
  const qs = query.toString();
  return apiRequest<CrontabList>(qs ? `${apiV1}/jobs/crontabs?${qs}` : `${apiV1}/jobs/crontabs`);
}
