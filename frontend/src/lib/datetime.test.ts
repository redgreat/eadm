import { describe, expect, it, vi } from "vitest";
import { daysAgo, hoursAgo, toApiDateTime, toInputDateTime } from "./datetime";

describe("datetime helpers", () => {
  it("calculates relative dates from the current time", () => {
    vi.setSystemTime(new Date("2026-09-27T08:00:00.000Z"));
    expect(daysAgo(2).toISOString()).toBe("2026-09-25T08:00:00.000Z");
    expect(hoursAgo(3).toISOString()).toBe("2026-09-27T05:00:00.000Z");
    vi.useRealTimers();
  });

  it("formats local input and API values", () => {
    const local = new Date(2026, 8, 27, 9, 5);
    expect(toInputDateTime(local)).toBe("2026-09-27T09:05");
    expect(toApiDateTime("2026-09-27T09:05")).toBe("2026-09-27 09:05:00");
  });
});
