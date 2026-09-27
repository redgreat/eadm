import { apiRequest, apiV1 } from "./client";
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
  return apiRequest<HealthList>(`${apiV1}/health/records?${query.toString()}`);
}
