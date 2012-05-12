using System;
using System.ComponentModel;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;
using System.Windows.Forms.DataVisualization.Charting;
using Microsoft.FSharp.Core;

namespace WinChart
{
    public partial class SimChart : Form
    {
        const int nTicks = 96;
        Sim.SimCar tSim;

        private const int nRealTime = 0;
        private const int nPowerNodes = 1;
        private const int nPhev = 2;
        private const int nDayAhead = 3;
        private const int nPhevsLeft = 4;

        private const int nPhevStatus = 0;
        private const int nPhevBattery = 1;
        private const int nPhevsPDF = 2;

        private const int nTrfCapacity = 0;
        private const int nTrfCurrent = 1;
        private const int nTrfFiltered = 2;

        private const int nDayaheadOriginal = 0;
        private const int nDayaheadPrev = 1;
        private const int nDayaheadCur = 2;
        private const int nDayaheadExp = 3;
        private const int nDayaheadSupervisor = 4;
        private const int nDayaheadAnts = 5;

        private static string[] series = { "Total", "PowerNodes", "PHEVs", "Dayahead", "PHEVs left (not fully charged)" };
        private static string[] _seriesPhev = { "PHEV status", "PHEV battery", "PHEV PDF" };
        private static string[] _seriesTrf = {"Capacity", "Current", "Unfiltered"};
        private static string[] _seriesDayahead = { "Original", "Dayahead previous", "Dayahead current", "Expected", "Supervisor", "Ants" };

        private static int counter = 0;
        private static int nstep = 0;

        private delegate void saveImageDelegate(string fileName);
        private delegate void updateChartDelegate(Chart chart, int i, Double[] points);
        private delegate void updateChartSaveImageDelegate(Chart chart, int i, Double[] points, bool saveImage, String path);
        private delegate void resetChartDelegate(Chart chart, int i, int j);
        private delegate void resetChartSaveImageDelegate(Chart chart, int i, int j, bool saveImage, String path);
        private delegate void updatePDFDelegate(Double[] chart);
        private delegate void addPointDelegate(Chart chart, int i, Double point);
        private delegate void incrementPointDelegate(Chart chart, int i, int point, Double val);
        private delegate void updatePointDelegate(Chart chart, int i, int point, Double val);
        private delegate void startDelegate();
        private delegate void updateLogDelegate(String ev);
        

        void saveImageControl(string fileName)
        {


        }

        void updateChart(Chart chart, int series, Double[] points, bool saveImage, String path)
        {
            if (chart.InvokeRequired)
            {
                chart.Invoke(new updateChartSaveImageDelegate(updateChart), new object[] { chart, series, points, saveImage, path });
            }
            else
            {
                updateChart(chart, series, points);

                if (saveImage)
                    chart.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}\\{1}.png", path, counter++), ChartImageFormat.Png);
                
                //for (int j = 0; j < chart.Series.Count; j++)
                //    chart.Series[j].Points.Clear();
            }

        }

        void updateChart(Chart chart, int i, Double[] points)
        {
            if (chart.InvokeRequired)
            {
                chart.Invoke(new updateChartDelegate(updateChart), new object[] { chart, i, points });
            }
            else
            {
                if (chart.Series[i].Points.Count > 0)
                    chart.Series[i].Points.Clear();

                for (int j = 0; j < points.Length; j++)
                    chart.Series[i].Points.Add(points[j]);
            }
        }

        void updatePDF(Double[] prob)
        {
            if (chart2.InvokeRequired)
            {
                chart2.BeginInvoke(new updatePDFDelegate(updatePDF), new object[] { prob });
            }
            else
            {
                resetChart(chart2, 0, 2);

                for (int i = 0; i < prob.Length; i++)
                {
                    if (chart2.Series[nPhevsPDF].Points.Count <= i)
                        chart2.Series[nPhevsPDF].Points.Add(prob[i]);
                    else if (prob[i] > chart2.Series[nPhevsPDF].Points[i].YValues[0])
                        chart2.Series[nPhevsPDF].Points[i].SetValueY(prob[i]);
                }
                chart2.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\phev\\phev_pdf.png"), ChartImageFormat.Png);
                resetChart(chart2, nPhevsPDF, nPhevsPDF + 1);
            }
        }

        void addPoint(Chart chart, int i, Double point)
        {
            if (chart.InvokeRequired)
            {
                chart.BeginInvoke(new addPointDelegate(addPoint), new object[] { chart, i, point });
            }
            else
            {
                if (chart.Series[i].Points.Count >= 96)
                    chart.Series[i].Points.Clear();

                chart.Series[i].Points.Add(point);
            }
        }

        void updatePoint(Chart chart, int i, int point, double val)
        {
            if (chart.InvokeRequired)
            {
                chart.BeginInvoke(new updatePointDelegate(updatePoint), new object[] { chart, i, point, val });
            }
            else
            {
                if (chart.Series[i].Points.Count == 0)
                    for (int j = 0; j < 96; j++)
                    {
                        chart.Series[i].Points.Add(0);
                        chart.Series[i].Points[j].IsEmpty = true;
                    }
                if (point > 0 && point < 96)
                {
                    chart.Series[i].Points[point].SetValueY(val);
                    chart.Series[i].Points[point].IsEmpty = false;
                }
            }
        }

        void incrementPoint(Chart chart, int i, int point, double val)
        {
            if (chart.InvokeRequired)
            {
                chart.BeginInvoke(new incrementPointDelegate(incrementPoint), new object[] { chart, i, point, val });
            }
            else
            {
                if (chart.Series[i].Points.Count == 0)
                    for (int j = 0; j < 96; j++)
                        chart.Series[i].Points.Add(0);
                double temp = chart.Series[i].Points[point].YValues[0];
                chart.Series[i].Points[point].SetValueY(temp + val);
            }
        }

        void resetChart(Chart chart, int from, int to, bool saveImage, String path)
        {
            if (chart.InvokeRequired)
            {
                chart.Invoke(new resetChartSaveImageDelegate(resetChart), new object[] { chart, from, to, saveImage, path });
            }
            else
            {
                if (saveImage)
                    chart.SaveImage(String.Format("C:\\SimCar\\SimCar\\data\\img\\{0}\\{1}.png", path, counter++), ChartImageFormat.Png);

                resetChart(chart, from, to);
            }
        }

        void resetChart(Chart chart, int from, int to)
        {
            if (chart.InvokeRequired)
            {
                chart.Invoke(new resetChartDelegate(resetChart), new object[] { chart, from, to });
            }
            else
            {
                for (int j = from; j < to; j++)
                    chart.Series[j].Points.Clear();
            }
        }

        void trfCapacity_Changed(object sender, EventArgs e)
        {
            Double point = (Double)sender;

            addPoint(chart3, nTrfCapacity, point);
        }

        void trfCurrent_Changed(object sender, EventArgs e)
        {
            Double point = (Double)sender;

            //incrementPoint(chart2, nPhevBattery, point, 1.0);
            addPoint(chart3, nTrfCurrent, point);
        }

        void trfFiltered_Changed(object sender, EventArgs e)
        {
            Double point = (Double)sender;

            //incrementPoint(chart2, nPhevBattery, point, 1.0);
            addPoint(chart3, nTrfFiltered, point);
        }

        void phevStatus_Changed(object sender, EventArgs e)
        {
            Double point = (Double) sender;

            addPoint(chart2, nPhevStatus, point);
        }

        void phevLeft_Changed(object sender, EventArgs e)
        {
            Tuple<int, Double> chart = (Tuple<int, Double>)sender;

            incrementPoint(chart1, nPhevsLeft, chart.Item1, chart.Item2);
        }

        void phevBattery_Changed(object sender, EventArgs e)
        {
            Double point = (Double) sender;

            //incrementPoint(chart2, nPhevBattery, point, 1.0);
            addPoint(chart2, nPhevBattery, point);
        }

        void prob_Reset(object sender, EventArgs e)
        {

        }

        void prob_Calc(object sender, EventArgs e)
        {
            Double[] chart = (Double[]) sender;

            updatePDF(chart);
        }
        
        void phev_Changed(object sender, EventArgs e)
        {
            updateChart(chart1, nPhev, (Double[])sender);
        }

        void pnode_Changed(object sender, EventArgs e)
        {
            updateChart(chart1, nPowerNodes, (Double[])sender);
        }

        void total_Changed(object sender, EventArgs e)
        {
            updateChart(chart1, nRealTime, (Double[])sender, true, "power");
            resetChart(chart2, 0, 2, true, "phev");
            resetChart(chart3, 0, 3, true, "trf");
            resetChart(chart1, nPhevsLeft, nPhevsLeft+1);
        }

        void chart_moving_average_comparison_Changed(object sender, EventArgs e)
        {
            Double[][] chart = (Double[][])sender;

            chart1.Series[0].Points.Clear();

            for (int j = 0; j < chart.Length; j++)
                chart1.Series[0].Points.Add(chart[0][j]);
        }

        void dayahead_Init(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {0}", counter, nstep));

            updateChart(chartDayahead, nDayaheadOriginal, chart);
        }
        void dayahead_Changed(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chart1.Titles[0].Text = (String.Format("Iteration #{0} : Step # {1}", counter, nstep++));

            updateChart(chart1, nDayAhead, chart);
        }

        void dayahead_Step(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            this.chartDayahead.Titles[0].Text = (String.Format("Iteration #{0} : Step # {1}", counter, nstep));

            updateChart(chartDayahead, nDayaheadCur, chart);
        }

        void dayahead_Exp(object sender, EventArgs e)
        {
            Double[] chart = (Double[])sender;

            resetChart(chartDayahead, nDayaheadExp, nDayaheadExp);
            updateChart(chartDayahead, nDayaheadExp, chart, true, "dayahead");
        }

        void dayahead_Ant(object sender, EventArgs e)
        {
            Tuple<int[], Double[]> chart = (Tuple<int[], Double[]>)sender;

            resetChart(chartDayahead, nDayaheadAnts, nDayaheadAnts+1);
            for (int i=0;i<chart.Item1.Length;i++) {
                if (chart.Item1[i] > -1)
                    updatePoint(chartDayahead, nDayaheadAnts, chart.Item1[i], chart.Item2[chart.Item1[i]]);
            }
        }

        void dayahead_Supervisor(object sender, EventArgs e)
        {
            Double chart = (Double)sender;

            addPoint(chartDayahead, nDayaheadSupervisor, chart);
            //updateChart(chartDayahead, nDayaheadSupervisor, chart);
        }

        void updateLog(String ev)
        {
            if (textLog.InvokeRequired)
            {
                textLog.BeginInvoke(new updateLogDelegate(updateLog), new object[]{ ev });
            }
            else
            {
                textLog.AppendText(ev + "\n");
            }
        }

        void updateDebug(String ev)
        {
            if (textBoxDebug.InvokeRequired)
            {
                textBoxDebug.BeginInvoke(new updateLogDelegate(updateDebug), new object[] { ev });
            }
            else
            {
                textBoxDebug.AppendText(ev + "\n");
            }
        }

        void progress_Changed(object sender, EventArgs e)
        {
            String ev = (String)sender;
            updateLog(ev);
        }

        void debug_Changed(object sender, EventArgs e)
        {
            String ev = (String)sender;
            updateDebug(ev);
        }

        void error_Changed(object sender, EventArgs e)
        {
            String ev = (String)sender;
            updateDebug(ev);
        }

        void setChartLabels(Chart chart)
        {
            chart.ChartAreas[0].AxisX.CustomLabels.Add(23, 24, "06:00");
            chart.ChartAreas[0].AxisX.CustomLabels.Add(47, 48, "12:00");
            chart.ChartAreas[0].AxisX.CustomLabels.Add(71, 73, "18:00");
            chart.ChartAreas[0].AxisX.CustomLabels.Add(90, 95, "24:00");
        }

        void RegisterEvents(Sim.SimCar tSim)
        {
            //tSim.RegisterEvents();
            tSim.RegisterPhevBattery(phevBattery_Changed);
            tSim.RegisterPhevStatus(phevStatus_Changed);
            tSim.RegisterProgressTotal(total_Changed);
            tSim.RegisterProgressPhev(phev_Changed);
            tSim.RegisterProgressPnode(pnode_Changed);
            tSim.RegisterPhevLeft(phevLeft_Changed);
            tSim.RegisterProb(prob_Calc);
            tSim.RegisterProbReset(prob_Reset);
            tSim.RegisterDayaheadProgress(dayahead_Changed);
            tSim.RegisterDayaheadInit(dayahead_Init);
            tSim.RegisterDayaheadStep(dayahead_Step);
            tSim.RegisterDayaheadExpected(dayahead_Exp);
            tSim.RegisterDayaheadSupervisor(dayahead_Supervisor);
            tSim.RegisterDayaheadAnt(dayahead_Ant);
            //tSim.TestDayahead(nSim);
            tSim.RegisterTrfCapacity(trfCapacity_Changed);
            tSim.RegisterTrfCurrent(trfCurrent_Changed);
            tSim.RegisterTrfFiltered(trfFiltered_Changed);
            tSim.RegisterDebug(debug_Changed);
            tSim.RegisterError(error_Changed);
            tSim.RegisterProgress(progress_Changed);
        }

        //void UnregisterEvents()
        //{
        //    tSim.UnregisterPhevBattery(phevBattery_Changed);
        //    tSim.UnregisterPhevStatus(phevStatus_Changed);
        //    tSim.UnregisterProgressTotal(total_Changed);
        //    tSim.UnregisterProgressPhev(phev_Changed);
        //    tSim.UnregisterProgressPnode(pnode_Changed);
        //    tSim.UnregisterPhevFailed(phevFailed_Changed);
        //    //tSim.RegisterProb(prob_Calc);
        //    //tSim.RegisterProbReset(prob_Reset);
        //    tSim.UnregisterDayaheadProgress(dayahead_Changed);
        //    tSim.UnregisterDayaheadInit(dayahead_Init);
        //    tSim.UnregisterDayaheadStep(dayahead_Step);
        //    tSim.UnregisterDayaheadExpected(dayahead_Exp);
        //    tSim.UnregisterDayaheadSupervisor(dayahead_Supervisor);
        //    tSim.UnregisterDayaheadAnt(dayahead_Ant);
        //    //tSim.TestDayahead(nSim);
        //    tSim.UnregisterTrfCapacity(trfCapacity_Changed);
        //    tSim.UnregisterTrfCurrent(trfCurrent_Changed);
        //    tSim.UnregisterTrfFiltered(trfFiltered_Changed);
        //}

        public SimChart()
        {
            InitializeComponent();

            chartDayahead.Titles.Add("Dayahead");
            chart3.Titles.Add("Transformer");
            chart2.Titles.Add("PHEV");
            chart1.Titles.Add("Iteration # 0 : Step # 0");
            //chart1.Titles.Add("alpha = 0.3, theta = 0.9");

            chart1.Series[0].LegendText = series[nRealTime];
            chart1.Series.Add(series[nPowerNodes]);
            chart1.Series.Add(series[nPhev]);
            chart1.Series.Add(series[nDayAhead]);
            chart1.Series.Add(series[nPhevsLeft]);

            //chart1.ChartAreas.Add("ChartArea1");

            chart2.Series[0].LegendText = _seriesPhev[nPhevStatus];
            chart2.Series.Add(_seriesPhev[nPhevBattery]);
            chart2.Series.Add(_seriesPhev[nPhevsPDF]);

            chart3.Series[0].LegendText = _seriesTrf[nTrfCapacity];
            chart3.Series.Add(_seriesTrf[nTrfCurrent]);
            chart3.Series.Add(_seriesTrf[nTrfFiltered]);

            chartDayahead.Series[0].LegendText = _seriesDayahead[nDayaheadOriginal];
            chartDayahead.Series.Add(_seriesDayahead[nDayaheadPrev]);
            chartDayahead.Series.Add(_seriesDayahead[nDayaheadCur]);
            chartDayahead.Series.Add(_seriesDayahead[nDayaheadExp]);
            chartDayahead.Series.Add(_seriesDayahead[nDayaheadSupervisor]);
            chartDayahead.Series.Add(_seriesDayahead[nDayaheadAnts]);

            setChartLabels(chart1);
            setChartLabels(chart2);
            setChartLabels(chart3);
            setChartLabels(chartDayahead);
            
            Series[] seriesPhev = {chart2.Series[nPhevStatus], chart2.Series[nPhevBattery], chart2.Series[nPhevsPDF]};
            Series[] seriesArray = { chart1.Series[nRealTime], chart1.Series[nPowerNodes], chart1.Series[nPhev], chart1.Series[nDayAhead], chart1.Series[nPhevsLeft] };
            Series[] seriesTrf = {chart3.Series[nTrfCapacity], chart3.Series[nTrfCurrent], chart3.Series[nTrfFiltered] };
            Series[] seriesDayahead = { chartDayahead.Series[nDayaheadOriginal], chartDayahead.Series[nDayaheadPrev], chartDayahead.Series[nDayaheadCur], chartDayahead.Series[nDayaheadExp], chartDayahead.Series[nDayaheadSupervisor], chartDayahead.Series[nDayaheadAnts] };

            // customize series
            seriesArray[nRealTime].ChartType = SeriesChartType.Line;
            seriesArray[nRealTime].Color = Color.Red;
            seriesArray[nRealTime].BorderWidth = 2;
            seriesArray[nRealTime].BorderDashStyle = ChartDashStyle.Solid;

            seriesArray[nPowerNodes].ChartType = SeriesChartType.Line;
            seriesArray[nPowerNodes].Color = Color.Blue;
            seriesArray[nPowerNodes].BorderWidth = 2;
            seriesArray[nPowerNodes].BorderDashStyle = ChartDashStyle.Dot;

            seriesArray[nPhev].ChartType = SeriesChartType.Line;
            seriesArray[nPhev].Color = Color.Crimson;
            seriesArray[nPhev].BorderWidth = 2;
            seriesArray[nPhev].BorderDashStyle = ChartDashStyle.Dot;

            seriesArray[nPhevsLeft].ChartType = SeriesChartType.Line;
            seriesArray[nPhevsLeft].Color = Color.Black;
            seriesArray[nPhevsLeft].BorderWidth = 2;
            seriesArray[nPhevsLeft].BorderDashStyle = ChartDashStyle.Dash;

            seriesArray[nDayAhead].ChartType = SeriesChartType.Line;
            seriesArray[nDayAhead].Color = Color.Sienna;
            seriesArray[nDayAhead].BorderWidth = 1;
            seriesArray[nDayAhead].BorderDashStyle = ChartDashStyle.Dot;

            seriesPhev[nPhevStatus].ChartType = SeriesChartType.StepLine;
            seriesPhev[nPhevStatus].Color = Color.Red;
            seriesPhev[nPhevStatus].BorderWidth = 2;
            seriesPhev[nPhevStatus].BorderDashStyle = ChartDashStyle.Dash;

            seriesPhev[nPhevBattery].ChartType = SeriesChartType.StepLine;
            seriesPhev[nPhevBattery].Color = Color.Blue;
            seriesPhev[nPhevBattery].BorderWidth = 2;
            seriesPhev[nPhevBattery].BorderDashStyle = ChartDashStyle.Dash;

            seriesPhev[nPhevsPDF].ChartType = SeriesChartType.StepLine;
            seriesPhev[nPhevsPDF].Color = Color.Green;
            seriesPhev[nPhevsPDF].BorderWidth = 2;
            seriesPhev[nPhevsPDF].BorderDashStyle = ChartDashStyle.Dash;

            seriesTrf[nTrfCapacity].ChartType = SeriesChartType.StepLine;
            seriesTrf[nTrfCapacity].Color = Color.Red;
            seriesTrf[nTrfCapacity].BorderWidth = 2;
            seriesTrf[nTrfCapacity].BorderDashStyle = ChartDashStyle.Dash;

            seriesTrf[nTrfCurrent].ChartType = SeriesChartType.Line;
            seriesTrf[nTrfCurrent].Color = Color.Blue;
            seriesTrf[nTrfCurrent].BorderWidth = 2;
            seriesTrf[nTrfCurrent].BorderDashStyle = ChartDashStyle.Dash;

            seriesTrf[nTrfFiltered].ChartType = SeriesChartType.Line;
            seriesTrf[nTrfFiltered].Color = Color.Sienna;
            seriesTrf[nTrfFiltered].BorderWidth = 2;
            seriesTrf[nTrfFiltered].BorderDashStyle = ChartDashStyle.Solid;

            seriesDayahead[nDayaheadOriginal].ChartType = SeriesChartType.Line;
            seriesDayahead[nDayaheadOriginal].Color = Color.Sienna;
            seriesDayahead[nDayaheadOriginal].BorderWidth = 2;
            seriesDayahead[nDayaheadOriginal].BorderDashStyle = ChartDashStyle.Dash;

            seriesDayahead[nDayaheadPrev].ChartType = SeriesChartType.Line;
            seriesDayahead[nDayaheadPrev].Color = Color.Blue;
            seriesDayahead[nDayaheadPrev].BorderWidth = 2;
            seriesDayahead[nDayaheadPrev].BorderDashStyle = ChartDashStyle.Solid;

            seriesDayahead[nDayaheadCur].ChartType = SeriesChartType.Line;
            seriesDayahead[nDayaheadCur].Color = Color.Red;
            seriesDayahead[nDayaheadCur].BorderWidth = 2;
            seriesDayahead[nDayaheadCur].BorderDashStyle = ChartDashStyle.Solid;

            seriesDayahead[nDayaheadExp].ChartType = SeriesChartType.Line;
            seriesDayahead[nDayaheadExp].Color = Color.Green;
            seriesDayahead[nDayaheadExp].BorderWidth = 2;
            seriesDayahead[nDayaheadExp].BorderDashStyle = ChartDashStyle.Solid;

            seriesDayahead[nDayaheadSupervisor].ChartType = SeriesChartType.Point;
            seriesDayahead[nDayaheadSupervisor].Color = Color.Black;
            seriesDayahead[nDayaheadSupervisor].BorderWidth = 2;
            seriesDayahead[nDayaheadSupervisor].BorderDashStyle = ChartDashStyle.Solid;

            seriesDayahead[nDayaheadAnts].ChartType = SeriesChartType.Point;
            seriesDayahead[nDayaheadAnts].Color = Color.Black;
            seriesDayahead[nDayaheadAnts].BorderWidth = 2;
            seriesDayahead[nDayaheadAnts].BorderDashStyle = ChartDashStyle.Solid;

            comboBox1.SelectedItem = "Random";
            comboBox2.SelectedItem = "Mixed";
            comboBox3.SelectedItem = "Expected";
            
            if (comboBox1.InvokeRequired)
            {
                comboBox1.Invoke(new startDelegate(Start));
            }
            else
            {
                tSim = new Sim.SimCar(nTicks);
                tSim.Init();
                RegisterEvents(tSim);
            }
        }

        public void Start()
        {
            Double distanceTheta = 1.0, shavingTheta = 0.99, shavingAlpha = 0.2;
            int phevLearningWindow = 40, nDays = 10;

            Int32.TryParse(textBoxPhevLearning.Text, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out phevLearningWindow);
            Int32.TryParse(textBoxDays.Text, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out nDays);
            Double.TryParse(textBoxDistanceTheta.Text, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out distanceTheta);
            Double.TryParse(textBoxShavingAlpha.Text, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out shavingAlpha);
            Double.TryParse(textBoxShavingTheta.Text, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out shavingTheta);

            button1.Enabled = false;

            Sim.Method method = null;
            Sim.Contribution contr = null;
            Sim.Scheduler scheduler = null;

            switch (comboBox1.SelectedItem.ToString())
            {
                case "Peak-shaving":
                    method = Sim.Method.Shaving;
                    break;
                case "Distance-rule":
                    method = Sim.Method.Distance;
                    break;
                case "Swarm":
                    method = Sim.Method.Swarm;
                    break;
                case "Random":
                    method = Sim.Method.Random;
                    break;
                case "Mixed":
                    method = Sim.Method.Mixed;
                    break;
            }
            switch (comboBox2.SelectedItem.ToString())
            {
                case "Proactive":
                    scheduler = Sim.Scheduler.Proactive;
                    break;
                case "Reactive":
                    scheduler = Sim.Scheduler.Reactive;
                    break;
                case "Random":
                    scheduler = Sim.Scheduler.Random;
                    break;
                case "Mixed":
                    scheduler = Sim.Scheduler.Mixed;
                    break;
            }
            switch (comboBox3.SelectedItem.ToString())
            {
                case "Expected":
                    contr = Sim.Contribution.Expected;
                    break;
                case "Simulated":
                    contr = Sim.Contribution.Simulated;
                    break;
            }

            tSim.PhevWindow = phevLearningWindow;
            tSim.Scheduler = scheduler;
            tSim.DistanceTheta = distanceTheta;
            tSim.ShavingAlpha = shavingAlpha;
            tSim.ShavingTheta = shavingTheta;
            tSim.Method = method;
            tSim.Contribution = contr;
            tSim.Days = nDays;

            BackgroundWorker bgWorker;
            bgWorker = new BackgroundWorker();
            bgWorker.DoWork += new DoWorkEventHandler(Simulation_Start);
            bgWorker.RunWorkerCompleted += new RunWorkerCompletedEventHandler(Simulation_Completed);
            bgWorker.RunWorkerAsync(tSim);
        }

        private void Simulation_Start(object sender, DoWorkEventArgs args)
        {
            Sim.SimCar tSim = (Sim.SimCar)args.Argument;
            tSim.ComputeDayahead();
            resetChart(chart1, 0, chart1.Series.Count);
            resetChart(chart2, 0, chart2.Series.Count);
            tSim.Run();
        }

        private void Simulation_Completed(object sender, RunWorkerCompletedEventArgs args)
        {
            button1.Enabled = true;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            //Thread oThread = new Thread(new ThreadStart(Start));
            //oThread.Start();
            Start();
        }

        private void comboBox3_SelectedIndexChanged(object sender, EventArgs e)
        {

        }

        private void textBox2_TextChanged(object sender, EventArgs e)
        {

        }

        private void comboBox2_SelectedIndexChanged(object sender, EventArgs e)
        {

        }
    }
}
