type Part = {
    x: number;
    m: number;
    a: number;
    s: number;
  };
  
  type Rule = {
    rating: string;
    operator: string;
    value: number;
    next: string;
  };
  
  type WorkFlow = Rule[];
  type WorkFlows = Record<string, WorkFlow>;
  type Range = Record<string, [number, number]>;
  
  const parseWorkFlows = (workFlowsRaw: string): WorkFlows =>
    workFlowsRaw
      .split("\n")
      .map((workFlow): [string, WorkFlow] => {
        const [name, rulesRaw] = workFlow.split("{");
        const rules = rulesRaw.split(",").map((rule) => {
          if (rule[rule.length - 1] === "}") {
            return {
              rating: null,
              operator: null,
              value: null,
              next: rule.slice(0, -1),
            };
          }
  
          const [, rating, operator, value, next] = rule.match(
            /([xmas])([<>=])(\d+):(\w+)/
          );
          return { rating, operator, value: Number(value), next };
        });
  
        return [name, rules];
      })
      .reduce((acc, [name, workFlow]) => {
        acc[name] = workFlow;
        return acc;
      }, {});
  
  const parseParts = (partsRaw: string) =>
    partsRaw
      .split("\n")
      .map((part) =>
        part
          .replaceAll(/[{}]/g, "")
          .split(",")
          .map((p) => p.split("="))
      )
      .map(
        (part): Part =>
          part.reduce((acc, [key, value]) => {
            acc[key] = Number(value);
            return acc;
          }, {} as Part)
      );
  
  const isAccepted = (
    part: Part,
    workFlowName: string,
    workFlows: WorkFlows
  ): boolean => {
    if (workFlowName === "R") return false;
    if (workFlowName === "A") return true;
  
    for (const rule of workFlows[workFlowName]) {
      if (
        rule.operator === null ||
        (rule.operator === "<" && part[rule.rating] < rule.value) ||
        (rule.operator === ">" && part[rule.rating] > rule.value)
      )
        return isAccepted(part, rule.next, workFlows);
    }
  
    throw new Error("No rule found");
  };
  
  export const getPartRatingSum = (input: string) => {
    const [workFlowsRaw, partsRaw] = input.split("\n\n");
  
    const workFlows = parseWorkFlows(workFlowsRaw);
    const parts = parseParts(partsRaw);
  
    return parts
      .filter((part) => isAccepted(part, "in", workFlows))
      .reduce((acc, part) => acc + part.x + part.m + part.s + part.a, 0);
  };
  
  const copyRange = (range: Range) => JSON.parse(JSON.stringify(range));
  
  const getRanges = (
    workFlows: WorkFlows,
    workFlowName: string,
    range: Range
  ): Range[] => {
    if (workFlowName === "R") return [];
    if (workFlowName === "A") return [copyRange(range)];
  
    const workFlow = workFlows[workFlowName];
  
    const ranges = [];
  
    for (const rule of workFlow) {
      if (rule.operator === null) {
        ranges.push(...getRanges(workFlows, rule.next, copyRange(range)));
      }
  
      if (rule.operator === "<") {
        const newRange = copyRange(range);
        newRange[rule.rating][1] = rule.value - 1;
  
        ranges.push(...getRanges(workFlows, rule.next, newRange));
  
        range[rule.rating][0] = rule.value;
      }
  
      if (rule.operator === ">") {
        const newRange = copyRange(range);
        newRange[rule.rating][0] = rule.value + 1;
  
        ranges.push(...getRanges(workFlows, rule.next, newRange));
  
        range[rule.rating][1] = rule.value;
      }
    }
  
    return ranges;
  };
  
  export const getRatingCombinations = (input: string) => {
    const workFlows = parseWorkFlows(input.split("\n\n")[0]);
  
    const range: Range = {
      x: [1, 4000],
      m: [1, 4000],
      a: [1, 4000],
      s: [1, 4000],
    };
  
    return getRanges(workFlows, "in", range)
      .map((range) =>
        Object.values(range).reduce((acc, [min, max]) => acc * (max - min + 1), 1)
      )
      .reduce((acc: number, v: number) => acc + v, 0);
  };

console.log('Part 1:', getPartRatingSum(Deno.readTextFileSync("input.txt")));
console.log('Part 2:', getRatingCombinations(Deno.readTextFileSync("input.txt")));