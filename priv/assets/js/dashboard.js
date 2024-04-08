/*!
 *
 * @author wangcw
 * @copyright (C) 2024, REDGREAT
 * Created : 2024-04-04 23:41:41
 *
 * Module : dashboard.js
 *
 */

function loadChart() {
    let location = $('#location');
    let finance = $('#finance');

    $.getJSON('/data/dashboard', function (resdata) {

        console.log("resdata: " + JSON.stringify(resdata));

        $('#locationWeek').text(resdata[0]);
        $('#stepsWeek').text(resdata[1]);
        $('#sleepWeek').text(resdata[2]);
        $('#heartWeek').text(resdata[3]);

        $('#locationYear').text(resdata[4]);
        $('#financeYear').text(resdata[5]);
        $('#sleepYear').text(resdata[6]);
        $('#heartYear').text(resdata[7]);

        let locationChart = new Chart(location, {
            type: 'bar',
            data: {
                labels: resdata[8],
                datasets: [{
                    data: resdata[9],
                    backgroundColor: "rgba(48, 164, 255, 0.2)",
                    borderColor: "rgba(48, 164, 255, 0.8)",
                    fill: true,
                    borderWidth: 1
                }]
            },
            options: {
                scales: {
                    yAxes: [{
                        ticks: {
                            min: 0,
                            beginAtZero: true
                        }
                    }],
                },
                animation: {
                    duration: 2000,
                    easing: 'easeOutQuart',
                },
                plugins: {
                    legend: {
                        display: false,
                        position: 'right',
                    },
                    title: {
                        display: true,
                        text: '里程数(km)',
                        position: 'left',
                    },
                },
            }
        });

        let financeChart = new Chart(finance, {
            type: 'line',
            data: {
                labels: resdata[10],
                datasets: [{
                    label: '收入',
                    data: resdata[11],
                    backgroundColor: "rgba(76, 175, 80, 0.5)",
                    borderColor: "#6da252",
                    borderWidth: 1,
                }, {
                    label: '支出',
                    data: resdata[12],
                    backgroundColor: "rgba(244, 67, 54, 0.5)",
                    borderColor: "#f44336",
                    borderWidth: 1,
                }]
            },
            options: {
                scales: {
                    yAxes: [{
                        ticks: {
                            min: 0,
                            beginAtZero: true
                        }
                    }],
                },
                animation: {
                    duration: 2000,
                    easing: 'easeOutQuart',
                },
                plugins: {
                    legend: {
                        display: false,
                        position: 'right',
                    },
                    title: {
                        display: true,
                        text: '金额(元)',
                        position: 'left',
                    },
                },
            }
        });
    }
)}

loadChart();
