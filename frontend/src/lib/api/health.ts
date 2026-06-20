import { apiRequest } from "./client";
import type { ApiResponse } from "./client";

export type HealthRecord = Record<string, string | number | boolean | null>;

export type HealthList = {
  items: HealthRecord[];
  total: number;
};

export function getHealthRecords(params: {
  dataType: string;
  startTime: string;
  endTime: string;
}): Promise<ApiResponse<HealthList>> {
  const query = new URLSearchParams(params);
  return apiRequest<HealthList>(`/api/health?${query.toString()}`);
}
