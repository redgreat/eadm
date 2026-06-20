import { apiRequest } from "./client";
import type { ApiResponse } from "./client";

export type FinanceRecord = {
  id: number | string;
  sourceType: number;
  inOrOut: string;
  tradeType: string;
  amount: string | number;
  tradeTime: string;
};

export type FinanceList = {
  items: FinanceRecord[];
  total: number;
};

export function getFinanceRecords(params: {
  sourceType: string;
  inOrOut: string;
  startTime: string;
  endTime: string;
}): Promise<ApiResponse<FinanceList>> {
  const query = new URLSearchParams(params);
  return apiRequest<FinanceList>(`/api/finance?${query.toString()}`);
}
